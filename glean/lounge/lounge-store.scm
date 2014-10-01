;;; glean --- fast learning tool.         -*- coding: utf-8 -*-

;;;; Lounge Store — Lounge → Filesystem Interface

;; Copyright © 2012, 2014 Alex Sassmannshausen
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;;; Commentary:
;;;
;;; Provide functionality to manage lounge state and to interface with
;;; the filesystem.
;;;
;;;; Code:

(define-module (glean lounge lounge-store)
  #:use-module (glean common monads)
  #:use-module (glean common utils)
  #:use-module (glean lounge lounge-filesystem)
  #:use-module (glean lounge profiles)
  #:use-module (glean lounge scorecards)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (rnrs records inspection)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (
            extract
            statef

            lounge?
            lounge-profiles
            lounge-tokens
            store-profile
            pdiff
            pdiff-hash
            pdiff-profile
            lounge-monad
            token?

            login
            authenticate
            fetch-lounge
            update-lounge
            register-profile
            view-profile
            modify-profile
            delete-profile
            purge-profile
            scorecard-diff
            scorecard-next

            profile-from-token
            missing-blobs
            ))

(define-record-type <lounge>
  (make-lounge profiles tokens)
  lounge?
  (profiles lounge-profiles)
  (tokens   lounge-tokens))

(define (make-diff hash field value)
  (list 'diff hash field value))
(define (diff? obj)
  (match obj
    (('diff (? string?) (? symbol?) value)
     #t)
    (_ #f)))

(define-record-type <tk-entry>
  (tk-entry hash time)
  tk-entry?
  (hash tk-hash)
  (time tk-time))
(define (mk-time ctime) (+ ctime (timeout)))
(define (tk-expired? tk-time ctime)
  (<= tk-time ctime))
(define timeout (const 600))

;;; for monads:
;; synonym for stateful, to force list around value(s)
(define* (statef value #:optional (state 'unimportant))
  "Return a stateful with VALUE wrapped in a list and a default STATE
of 'unimportant."
  (stateful (list value) state))

(define (lounge-monad-dict stateful level)
  (match (car (result stateful))
    ((? token? tk)  (list 'authenticate "Token:" tk)) ; authenticate
    (($ <lounge> profiles tks)                        ; fetch-lounge
     (list 'fetch-lounge "Lounge:"
           (string-append (number->string (vlist-length profiles))
                          " Profile(s) & "
                          (number->string (vlist-length tks))
                          " Token(s)")))
    (((? blobhash?) . (? number?))
     (list 'hash/counter-pair "Challenge:" (car (result stateful))))
    (((? string? name) (? string? lng) (? string? lib) ; view-profile
      (? list? active-modules))
     (list 'view-profile "Profile:"
           (if (> level 5) 
               (string-join (list name lng lib
                                  (object->string active-modules))
                            ", ")
               name)))
    (#f
     (list 'modify-profile "Updated:" 'ok))       ; modify-profile
    ('purge-ok
     (list 'purge-profile "Purged:" 'ok))
    (('diff (? string?) 'score ((? blobhash?) . (? boolean? evaluation)))
     (list 'scorecard-diff "Updating Scorecard:" evaluation))
    (('diff (? string?) 'meta (name #t lng lib #f))
     (list 'register-profile "Registering:" name))
    (('diff (? string?) 'meta ((? string? name) (? boolean?) #f #f new-phash))
     (list 'modify-profile "Updating name:" name))
    (('diff (? string?) 'meta (#f #t #f #f new-phash))
     (list 'modify-profile "Updating password:" new-phash))
    (('diff (? string?) 'meta (#f #f #f (? string? lib) #f))
     (list 'modify-profile "Updating library:" lib))
    (('diff (? string?) 'meta ())
     (list 'delete-profile "Deleting:" 'ok))
    (('diff (? string?) 'active-modules ((? pair?) ...))
     (list 'modify-profile "Activating:" 'pairs))
    (('diff (? string?) 'active-modules ((? symbol?) (? pair?) ...))
     (list 'modify-profile "De-Activating:" 'pairs))
    (('diff (? string?) 'hashmap hashmap)
     (list 'modify-profile "Enabling:" hashmap))
    ((? profile?)
     (list 'update-lounge "Modification:" 'ok))
    ((? nothing? noth)
     (let* ((id  (nothing-id noth))   ; Nothing msg
            (src (match id            ; Deduce src from id
                   ('username-taken 'register-profile)
                   ('unknown-user   'login)
                   ('invalid-token  'authenticate)
                   (_ 'unknown))))
       (list src "Nothing:"
             (if (> level 5)
                 (cons id (nothing-context noth))
                 id))))               ; -> log msg.
    (_ (list 'unknown "Result:" stateful))))

;; synonym for result, to take into account nothing possibility.
(define (extract st8teful)
  "Retrieve the result from ST8TEFUL, taking into account nothings. If
result contains only one item, extract that from the result list."
  (let ((prelim (result st8teful)))
    (if (null? (cdr prelim))
        (if (nothing? (car prelim))
            nothing
            (car prelim))
        prelim)))

;;;; Lounge Monad
;; A specialised monad providing logging, exception (FIXME: and file
;; locking) management.
(define (lounge-return . args)
  "Return a store mvalue seeded with ARGS."
  (lambda (lng-dir)
    "Return a stateful created using args."
    (if (null? (cdr args))
        (statef (car args) lng-dir)
        (stateful args lng-dir))))

(define (lounge-bind mvalue mproc)
  "Return a lounge mvalue, in turn capable of returning the result of
applying MVALUE to MPROC."
  (lambda (lng-dir)
    (let* ((new-stateful (mvalue lng-dir)) ; generate next stateful
           (reslt        (result new-stateful)))
      ((mlogger stateful? lounge-monad-dict) new-stateful)
      (cond ((nothing? (car reslt)) (car reslt))
            ;; As lounge should never be modified by mvalue (that
            ;; would mean that lounge logic would be carrying out
            ;; state updates — these should be done at monad level),
            ;; the LNG passed to lounge-bind will be identical to LNG
            ;; coming out of new-stateful.
            (else (let ((next (apply mproc reslt)))
                    (if (procedure? next)
                        (next lng-dir)
                        next)))))))

(define-monad lounge-monad
  (bind   lounge-bind)
  (return lounge-return))

;;;;; Monadic Procedures

(define (login hash)
  "Return a lounge mvalue which, when resolved, returns a new token
for HASH if the user identified by it is known or nothing if they are
not."
  (lambda (lng-dir)
    (let* ((tk  (lounge lng-dir hash))
           (ret (if tk tk (nothing 'unknown-user '()))))
      (statef ret lng-dir))))

(define (authenticate token)
  "Return a lounge mvalue which, when resolved, returns a new token if
TOKEN is currently a valid token, or nothing if it is not."
  (lambda (lng-dir)
    (let* ((tk  (lounge lng-dir token))
           (ret (if tk tk (nothing 'invalid-token (list token)))))
      (statef ret lng-dir))))

(define (scorecard-next token lounge)
  "Return a lounge mvalue which, when resolved, returns the next
eligible challenge for the profile associated with TOKEN in LOUNGE."
  (lambda (lng-dir)
    (let* ((profile (profile-from-token token lounge))
           (diff (missing-blobs profile)))
      (cond ((null? (profile-active-modules profile))
             (statef #f lng-dir))       ; no active-modules yet
            ;; If scorecard & active modules are in sync, proceed…
            ((null? diff)
             (statef (fetch-next-hash-counter-pair profile) lng-dir))
            ;; else force sync.
            (else
             (statef diff lng-dir))))))

(define (scorecard-diff token result lounge)
  "Return a lounge mvalue which, when resolved, returns a pdiff for
the profile associated with TOKEN in LOUNGE on the basis of the
challenge evaluation RESULT."
  (lambda (lng-dir)
    (let* ((profile  (profile-from-token token lounge))
           (blobhash (car (fetch-next-hash-counter-pair profile))))
      (statef (make-diff (hash-from-token token
                                          (lounge-tokens lounge))
                         'score
                         `(,blobhash . ,result))
              lng-dir))))

(define (register-profile name password lng-port lib-port lounge)
  "Return a lounge mvalue which, when resolved, returns a pdiff for
a new profile created using NAME, PASSWORD, LNG-PORT and LIB-PORT in
LOUNGE."
  (lambda (lng-dir)
    (if (name-taken? name (lounge-profiles lounge))
        (statef (nothing 'username-taken (list name)))
        (statef (make-diff (profile-hash name password)
                           'meta
                           `(,name #t ,lng-port ,lib-port #f))
                lng-dir))))

(define (view-profile token lounge)
  "Return a lounge-monad mvalue which, when resolved, will return the
profile stored in the lounge LOUNGE that corresponds to the token
TOKEN."
  (lambda (lng-dir)
    (let ((profile (profile-from-token token lounge)))
      (statef (list (profile-name           profile)
                    (profile-prof-server    profile)
                    (profile-mod-server     profile)
                    (profile-active-modules profile))
              lng-dir))))

(define (modify-profile token field value lounge)
  "Return a lounge mvalue which, when resolved, returns a pdiff for
the profile identified by TOKEN in LOUNGE, where FIELD has been
updated according to VALUE."
  (lambda (lng-dir)
    (let ((hash (hash-from-token token (lounge-tokens lounge))))
      (cond ((eqv? field 'scorecard)    ; Scorecard
             (statef (make-diff hash 'hashmap value)))
            ((eqv? field 'active-modules) ; Active-modules
             (statef (make-diff hash 'active-modules value)))
            ((eqv? field 'prof-server)  ; Profile server
             (statef (make-diff hash 'meta `(#f #f ,value #f #f))))
            ((eqv? field 'mod-server)   ; Module server
             (statef (make-diff hash 'meta `(#f #f #f ,value #f))))
            ((eqv? field 'name)         ; Name change
             (match value
               ((name . password)
                (cond ((name-taken? name (lounge-profiles lounge))
                       (statef (nothing 'username-taken `(,name))))
                      ;; double-check the supplied password.
                      ((wrong-password? password ; supplied password
                                        (profile-from-token token lounge)
                                        (lounge-profiles lounge))
                       (statef (nothing 'incorrect-password '())))
                      (else
                       (let ((newhash (profile-hash name password)))
                         (statef (make-diff newhash
                                            'meta
                                            `(,name #f #f #f ,hash))
                                 lng-dir)))))))
            ((eqv? field 'password)     ; Password
             (let* ((profile (profile-from-hash hash
                                                (lounge-profiles
                                                 lounge)))
                    (newhash (profile-hash (profile-name profile)
                                           value)))
               (statef (make-diff newhash
                                  'meta
                                  `(#f #t #f #f ,hash))
                       lng-dir)))

            (else                       ; No other field allowed.
             (statef (nothing 'unknown-field `(,field))))))))

(define (delete-profile token lounge)
  "Return a lounge mvalue which, when resolved, returns a pdiff for
the profile identified by TOKEN in LOUNGE requesting its deletion."
  (lambda (lng-dir)
    (let* ((tk-entry (cdr (vhash-assoc token
                                       (lounge-tokens lounge))))
           (hash     (tk-hash tk-entry)))
      (statef (make-diff hash 'meta '()) lng-dir))))

(define (purge-profile token)
  "Return a lounge mvalue which, when resolved, forces the lounge to
destroy TOKEN in its token table."
  (lambda (lng-dir)
    (statef (lounge lng-dir 'purge #:token token) lng-dir)))

(define (fetch-lounge)
  "Return a lounge mvalue which, when resolved, returns a lounge."
  (lambda (lng-dir)
    (statef (lounge lng-dir) lng-dir)))

(define (update-lounge diff save?)
  "Return a lounge mvalue which, when resolved, updates the lounge on
the basis of DIFF. The return value is irrelevant."
  (lambda (lng-dir)
    (statef (lounge lng-dir diff #:save? save?) lng-dir)))

;;;;; I/O Lounge Store Operations
;; A lounge is a database of all known profiles stored in a vhash
;; table. A secondary index of tokens to hashes is maintained in the
;; lounge to manage transactional authentication.

;; FIXME: The current implementation of lounge is likely to become a
;; serious bottle-neck and requires further consideration. At the very
;; least it might be worth considering to split tokens into a
;; separately maintained procedure, allowing for more parrallelism.
(define lounge
  (let ((profiles vlist-null)
        (tokens   vlist-null))
    (lambda* (lng-dir #:optional operation #:key (save? #f)
                      (token #f))
      (if (vlist-null? profiles)
          (set! profiles (compile-lounge lng-dir)))
      (cond ((not operation) (make-lounge profiles tokens))
            ;; Update Profile
            ((diff? operation)
             ;; use futures to write to disk as well as set!
             (if save? (write-diff operation lng-dir (current-time)))
             (match (store-profile operation profiles)
               ((profile . profilez)
                (set! profiles profilez)
                profile)
               (_ (error "lounge -- Cache unsuccessfull."))))
            ;; Delete Token (Delete Profile step 2)
            ((eqv? operation 'purge)
             (set! tokens (vhash-delete token tokens))
             'purge-ok)
            ;; Authenticate
            ((token?        operation)
             (let ((tk-pair (renew-tk operation
                                      tokens
                                      (current-time))))
               (if tk-pair
                   (begin
                     (set! tokens (car tk-pair))
                     (cdr tk-pair))
                   #f)))
            ;; Login
            (else
             (let ((tk-pair (fresh-tk operation
                                      tokens
                                      profiles
                                      (current-time))))
               (if tk-pair
                   (begin
                     (set! tokens (car tk-pair))
                     (cdr tk-pair))
                   #f)))))))

;;;;; Safe I/O Helpers
(define (store-profile diff profiles)
  "Return a new profiles vhash based on PROFILES, taking into account
instructions carried by HASH, FIELD and VALUE."
  (match diff
    (('diff hash field value)
     (cond ((eqv? field 'score)
            (match value
              ((blobhash . (? boolean? result))
               (let* ((oldprofile (profile-from-hash hash profiles))
                      (oldscc     (profile-scorecard oldprofile)))
                 (save hash
                       (update-profile 'scorecard
                                       (update-scorecard oldscc
                                                         blobhash
                                                         result)
                                       oldprofile)
                       (vhash-delete hash profiles))))
              (_ (error "store-profile -- invalid score value"))))
           ((eqv? field 'meta)
            (match value
              ((name password lounge library oldhash)
               (modify-meta hash name password lounge library oldhash
                            profiles))
              (() (cons #f (vhash-delete hash profiles))) ; Delete profile
              (_  (error "store-profile -- invalid meta value"))))
           ((eqv? field 'active-modules)
            (match value
              ((((? blobhash?) . (? blobhash?)) ...)
               (let* ((oldprofile (profile-from-hash hash profiles))
                      (actives    (profile-active-modules oldprofile)))
                 (save hash
                       (update-profile
                        'active-modules
                        (fold (lambda (mod-pair act-mods)
                                (if (member mod-pair act-mods)
                                    act-mods
                                    (cons mod-pair act-mods)))
                              actives
                              value)
                        oldprofile)
                       (vhash-delete hash profiles))))
              ;; FIXME: Quick 'n dirty active-mods de-activation: will
              ;; not clean the scorecard, which should happen!
              (('negate ((? blobhash?) . (? blobhash?)) ...)
               (let* ((oldprofile (profile-from-hash hash profiles))
                      (actives    (profile-active-modules oldprofile)))
                 (save hash
                       (update-profile
                        'active-modules
                        (lset-difference equal? actives (cdr value))
                        oldprofile)
                       (vhash-delete hash profiles))))
              (_ (error "store-profile -- invalid active-modules"))))
           ((eqv? field 'hashmap)
            (let* ((blobs      (hashmap->blobs value))
                   (oldprofile (profile-from-hash hash profiles))
                   (scorecard  (profile-scorecard oldprofile)))
              (save hash
                    (update-profile 'scorecard
                                    (add-blobs blobs scorecard)
                                    oldprofile)
                    (vhash-delete hash profiles))))
           (else (error "store-profile -- invalid field."))))))

(define (save hash profile profiles)
  (cons profile (vhash-cons hash profile profiles)))

(define (modify-meta hash name password lounge library oldhash
                     profiles)
  (cond ((and name password lounge
              library)             ; Registration
         (save hash
               (make-bare-profile name lounge library)
               profiles))
        (lounge                    ; New lounge
         (let ((oldprofile (profile-from-hash hash profiles)))
           (save hash
                 (update-profile 'prof-server lounge
                                 oldprofile)
                 (vhash-delete hash profiles))))
        (library                   ; New library
         (let ((oldprofile (profile-from-hash hash profiles)))
           (save hash
                 (update-profile 'mod-server library
                                 oldprofile)
                 (vhash-delete hash profiles))))
        ((and name oldhash)        ; New name
         (let ((oldprofile (profile-from-hash oldhash profiles)))
           (save hash
                 (update-profile 'name name oldprofile)
                 (vhash-delete oldhash profiles))))
        ((and password oldhash)    ; New password
         (let ((oldprofile (profile-from-hash oldhash profiles)))
           (if  (not oldprofile)
                (error "modify-data -- failed to fetch oldprofile!")
                (save hash
                      oldprofile
                      (vhash-delete oldhash profiles)))))
        (else (error "modify-meta -- invalid values."))))

(define (fresh-tk hash tokens profiles time)
  "Return a new tokens/token pair if HASH corresponds to an entry in
PROFILES.  TIME is used to create the new tk-entry for HASH in the
tokens vhash based on TOKENS.  If HASH cannot be found in PROFILES,
return #f."
  (if (vhash-assoc hash profiles)
      (let ((tk  (make-token hash time))
            (tks (clear-tokens hash tokens time)))
        (cons (vhash-cons tk (tk-entry hash (mk-time time)) tks) tk))
      #f))
(define (renew-tk token tokens time)
  "Return a new tokens/token pair if TOKEN corresponds to a token in
TOKENS and is not expired.  TIME is used to create the new
tk-entry. Return a tokens/#f pair if TOKEN in TOKENS is expired.  If
TOKEN cannot be found in TOKENS, return #f."
  (let ((auth-pair (vhash-assoc token tokens)))
    (cond ((and auth-pair
                (tk-expired? (tk-time (cdr auth-pair)) time))
           (cons (vhash-delete token tokens) #f))
          (auth-pair
           (let ((tk (make-token (tk-hash (cdr auth-pair)) time)))
             (cons (vhash-cons tk
                               (tk-entry (tk-hash (cdr auth-pair))
                                         (mk-time time))
                               (vhash-delete token tokens))
                   tk)))
          (else #f))))
(define (hash-from-token token tokens)
  "Return the hash associated with TOKEN in TOKENS."
  (let ((prelim (vhash-assoc token tokens)))
    (if prelim                          ; match
        (tk-hash (cdr prelim))          ; get hash
        #f)))                           ; no match
(define (profile-from-token token lounge)
  "Return the profile associated with TOKEN in LOUNGE or #f if TOKEN
does not identify a profile."
  (let* ((token   (hash-from-token token (lounge-tokens lounge)))
         (profile (if token
                      (vhash-assoc token (lounge-profiles lounge))
                      #f)))
    (if profile (cdr profile) #f)))
(define (profile-from-hash hash profiles)
  "Return the profile associated with HASH in PROFILES or #f if HASH
does not identify a profile."
  (and=> (vhash-assoc hash profiles)
         cdr))

(define (token? obj)
  "Return #t if OBJ satisfies all criteria for being considered a
valid token, #f otherwise."
  (number? obj))

(define (clear-tokens hash tokens time)
  "Return a new tokens table based on TOKENS with all entries of HASH
or all expired tokens (on the basis of TIME) removed."
  (vhash-fold (lambda (k v result)
                (if (or (string=?    (tk-hash v) hash)
                        (tk-expired? (tk-time v) time))
                    result
                    (vhash-cons k v result)))
              vlist-null
              tokens))

(define (make-token hash time)
  "Return a transactional token generated from HASH and TIME."
(random (* 2 time)
        (seed->random-state
         (string-append hash
                        (number->string time)))))

;;;;; Profile Management
;;;; Define the functions used to provide the functionality defined
;;;; above.
(define (missing-blobs profile)
  "Return a list of blobs not currently contained in the
scorecard of PROFILE. Used to check whether we need to request a new
hashmap."
  (define (check-blob-maker scorecard-data)
    (lambda (active-module results)
      (if (scorecard-data 'get (cdr active-module))
          results (cons active-module results))))

  (fold (check-blob-maker (scorecard-data
                           (profile-scorecard profile)))
             '()
             (profile-active-modules profile)))

(define (fetch-next-hash-counter-pair profile)
  "Return the next highest priority blobhash and the blob's counter
for PROFILE.

This is carried out in 2 steps: 1) locate the highest-priority
crown-blobhash, by comparing PROFILE's active-modules against the
crown-blobhash table. 2) locate that crown-blobhash's highest-priority
root-blobhash.

Finally the counter of the blob owning the blobhash is extracted and
appended to blobhash to create a pair.

NOTE: UPDATE-PROFILE will carry out a stateful side-effect of
updating PROFILE's parent in the store."
  (define (active-module-hashes active-modules)
    "Return a list containing the sethash for each entry in
ACTIVE-MODULES."
    (map cdr active-modules))
  (define (traverse-blob blob)
    "Return the highest priority blob at the root of BLOB (the
highest priority root-blob of BLOB)."
    (let ((children (blob-children blob)))
      (if (null? children)
          blob                          ; blob is a root-blob
          (traverse-blob (fold highest-priority-blob
                               (make-dummy-blob)
                               children)))))
  (define (highest-priority-blob current-blobhash winning-blob)
    "Return WINNING-BLOB if it has a higher priority than the blob
identified by CURRENT-BLOBHASH. Otherwise return the latter blob."
    (let ((current-blob (find-blob current-blobhash
                                   (profile-scorecard profile))))
      (if (lower-score? winning-blob current-blob)
          winning-blob
          current-blob)))

  ;; Initiate search with the crown-blobhashes provided by PROFILE's
  ;; active-modules list.
  (let ((selected-blob
         (traverse-blob
          (fold highest-priority-blob
                (make-dummy-blob)
                (active-module-hashes
                 (profile-active-modules profile))))))
    (if (dummy-blob? selected-blob)
        #f
        (cons (blob-hash selected-blob) (blob-counter selected-blob)))))

(define (name-taken? name profiles)
  "Return #t if NAME is already in use by a profile in
PROFILES. Return #f otherwise."
  (vhash-fold (lambda (hash profile found)
                (if (not found)
                    (string=? name (profile-name profile))
                    found))
              #f profiles))
(define (wrong-password? password profile profiles)
  "Return #t if the hash of PASSWORD and the name in PROFILE is not a
key in profiles. Return #f otherwise."
  (not (vhash-assoc (profile-hash (profile-name profile) password)
                    profiles)))


;;;;;; On Scorecards and Blobhashes
;;
;; FIXME: this commentary is partly obsolete.
;;
;; Scorecards are essentially a flat (hash?) table, associating a
;; given blobhash with the corresponding blob.
;;
;; Each blob contains the blobhash, the parent's blobhash (if
;; applicable, else #f), the children's blobhashes (if applicable,
;; else #f), the score associated with the blobhash and a counter,
;; indicating how many challenges have been provided for this
;; blobhash.
;;
;; In this manner, locating any specific blobhash can be done in
;; constant time.
;;
;; A separate, non-profile specific table exists, maintaining a list
;; of all known 'crown blobhashes', i.e. blobhashes that do not
;; themselves contain parents. In this way, when searching for the
;; lowest scoring blobhash we can search the 'crown-blobhashes' scores
;; for the lowest scoring crown-blobhash, and from there search each
;; child's score until we reach the lowest-scoring 'root-blobhash',
;; i.e. the blobhash that has no children — this is the blob-hash that
;; should be provided to mod-server to generate the next challenge.
;;
;; This search will take x * n score queries (constant time, see
;; above), where n is the number of blobhashes at any level
;; (crown-blobhashes and then each set of children), and x is the
;; number of levels along this path.  (This search is carried out by
;; (fetch-next-blobhash)).

(define (hashmap->blobs hashmap)
  "Return a list of blobs, by converting each hashtree in HASHMAP to blobs."
  (fold append '() (map hashtree->blobs hashmap)))

(define* (hashtree->blobs hashtree #:optional (parents '()))
  "Return a list of blobs by converting HASHTREE into blobs recursively."
  ;; FIXME: this procedure is currently not tail-recursive. It is also an
  ;; expensive operation in general and would benefit from being
  ;; re-factored. A lot.
  (define no-children '())
  (define (qblob name parents children properties)
    (make-blob name parents children 0 0 properties '()))
  (define (children subtrees) (map caar subtrees))
  (hashtree-map hashtree
                (lambda (hash properties subtrees)
                  (cons (qblob hash parents
                               (children subtrees)
                               properties)
                        (flatten
                         (map (lambda (subtree)
                                (hashtree->blobs subtree (list hash)))
                              subtrees))))
                (lambda (hash properties)
                  (list (qblob hash parents no-children properties)))
                (const #f)))

(define (hashtree-map hashtree branch-proc leaf-proc error-proc)
  (match hashtree
    (((hash . properties) subtrees)
     (branch-proc hash properties subtrees))
    (((hash . properties))
     (leaf-proc hash properties))
    (_ (error-proc))))

(define* (old-hashtree->blobs hashtree #:optional (parents '()))
  "Return a list of blobs by converting HASHTREE into blobs recursively."
  ;; FIXME: this procedure is currently not tail-recursive. It is also an
  ;; expensive operation in general and would benefit from being
  ;; re-factored. A lot.
  (define no-children '())
  (define (qblob name parents children properties)
    (make-blob name parents children 0 0 properties '()))
  (define (children subtrees) (map caar subtrees))
  (match hashtree
    (((hash . properties) subtrees)
     (cons (qblob hash parents
                  (children subtrees)
                  properties)
           (flatten
            (map (lambda (subtree)
                   (hashtree->blobs subtree (list hash)))
                 subtrees))))
    (((hash . properties))
     (list (qblob hash parents no-children properties)))
    (_ #f)))
