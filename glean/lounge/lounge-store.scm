;; lounge-store.scm --- lounge operational interface  -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
;;
;; This file is part of Glean.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Provide functionality to manage lounge state and to interface with
;; the filesystem.
;;
;; Part of the lounge-store's responsibility is the loading of the filesystem
;; components.  At present this is simply hard-coded to lounge-filesystem, but
;; this will be replaced with the 'component' subsystem.
;;
;;; Code:

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
  #:use-module (srfi srfi-26)
  #:export (
            store-profile
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


;;;;; Lounge and Lounge Monad Definition
;;; A lounge is a record-type which contains a vhash of profile-hashes ->
;;; profiles in its profiles field and a vhash of tokens -> tk-entry records
;;; in its tokens field.

(define-record-type <lounge>
  (make-lounge profiles tokens)
  lounge?
  (profiles lounge-profiles)
  (tokens   lounge-tokens))

;;; A tk-entry is a record type which contains a profile-hash in its hash
;;; field and an expiration time in its time field.

(define-record-type <tk-entry>
  (tk-entry hash time)
  tk-entry?
  (hash tk-hash)
  (time tk-time))

(define (mk-time ctime)
  "Return an expiration time value on the basis of CTIME."
  (define timeout (const 600))
  (+ ctime (timeout)))

(define (tk-expired? tk-time ctime)
  "Return #t if TK-TIME is expired, i.e. smaller than CTIME, #f otherwise."
  (<= tk-time ctime))

;;;; Diffs
;;; Diffs are transactional summary suitable for passing to the storage system
;;; as well as for updating the in-memory profile representations.

(define (make-diff hash field value)
  "Return a diff, a tagged list, containing HASH, FIELD and VALUE."
  (list 'diff hash field value))

(define (diff? obj)
  "Return #t if OBJ is a diff, #f otherwise."
  (match obj
    (('diff (? string?) (? symbol?) value) #t)
    (_ #f)))


;;;;; Lounge Monad
;;; A specialised monad providing logging and exception management.

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
  (let ((log (mlogger stateful? lounge-monad-dict)))
    (lambda (lng-dir)
      (let* ((new-stateful (mvalue lng-dir)) ; generate next stateful
             (reslt        (result new-stateful)))
        (log new-stateful (log-level))
        (cond ((nothing? (car reslt)) (car reslt))
              ;; As lounge should never be modified by mvalue (that
              ;; would mean that lounge logic would be carrying out
              ;; state updates — these should be done at monad level),
              ;; the LNG passed to lounge-bind will be identical to LNG
              ;; coming out of new-stateful.
              (else (let ((next (apply mproc reslt)))
                      (if (procedure? next)
                          (next lng-dir)
                          next))))))))

(define-monad lounge-monad
  (bind   lounge-bind)
  (return lounge-return))


;;;; Monad Helpers:

(define* (statef value #:optional (state 'unimportant))
  "Return a stateful with VALUE wrapped in a list and a default STATE
of 'unimportant."
  (stateful (list value) state))

(define (extract st8teful)
  "Retrieve the result from ST8TEFUL, taking into account nothings. If
result contains only one item, extract that from the result list."
  (let ((prelim (result st8teful)))
    (if (null? (cdr prelim))
        (if (nothing? (car prelim))
            nothing
            (car prelim))
        prelim)))

(define (lounge-monad-dict stateful level)
  "Interpret STATEFUL with regard to LEVEL, and return a list suitable for
interpretation by mlogger, to emit meaningful lounge-monad message.

Meaningful logging continues to be an issue.  This approach seems promising."
  (let ((long? (eqv? level 'all)))
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
             (if long?
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
               (if long?
                   (cons id (nothing-context noth))
                   id))))               ; -> log msg.
      (_ (list 'unknown "Result:" stateful)))))


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
    (let ((profile (profile-from-token token lounge)))
      (if (null? (profile-active-modules profile))
          (statef #f lng-dir)       ; no active-modules yet
          (let ((diff (missing-blobs profile)))
            (if (null? diff)
                ;; scorecard & active modules in sync: proceed...
                (statef (fetch-next-challenge-details profile) lng-dir)
                ;; force re-sync.
                (statef diff lng-dir)))))))

(define (scorecard-diff token result lounge)
  "Return a lounge mvalue which, when resolved, returns a diff for the profile
associated with TOKEN in LOUNGE on the basis of the challenge evaluation
RESULT."
  (lambda (lng-dir)
    (let* ((profile  (profile-from-token token lounge))
           (blobhash (car (fetch-next-hash-counter-pair profile))))
      (statef (make-diff (hash-from-token token
                                          (lounge-tokens lounge))
                         'score
                         `(,blobhash . ,result))
              lng-dir))))

(define (register-profile name password lng-port lib-port lounge)
  "Return a lounge mvalue which, when resolved, returns a diff for a new
profile created using NAME, PASSWORD, LNG-PORT and LIB-PORT in LOUNGE."
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
  "Return a lounge mvalue which, when resolved, returns a diff for the profile
identified by TOKEN in LOUNGE, where FIELD has been updated according to
VALUE."
  (lambda (lng-dir)
    (let ((hash (hash-from-token token (lounge-tokens lounge))))
      (match field
        ('scorecard (statef (make-diff hash 'hashmap value)))
        ('active-modules (statef (make-diff hash 'active-modules value)))
        ('prof-server (statef (make-diff hash 'meta `(#f #f ,value #f #f))))
        ('mod-server (statef (make-diff hash 'meta `(#f #f #f ,value #f))))
        ('name (match value
                 ((name . password)     ; guaranteed to be 2 strings.
                  (cond ((name-taken? name (lounge-profiles lounge))
                         (statef (nothing 'username-taken `(,name))))
                        ((wrong-password? password
                                          (profile-from-token token lounge)
                                          (lounge-profiles lounge))
                         (statef (nothing 'incorrect-password '())))
                        (else
                         (statef (make-diff (profile-hash name password) 'meta
                                            `(,name #f #f #f ,hash))
                                 lng-dir))))))
        ('password (statef
                    (make-diff (profile-hash
                                (profile-name
                                 (profile-from-hash hash
                                                    (lounge-profiles lounge)))
                                value)
                               'meta
                               `(#f #t #f #f ,hash))
                    lng-dir))
        (_ (statef (nothing 'unknown-field `(,field))))))))

(define (delete-profile token lounge)
  "Return a lounge mvalue which, when resolved, returns a diff for the profile
identified by TOKEN in LOUNGE requesting its deletion."
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
;;; A lounge is a database of all known profiles stored in a vhash table. A
;;; secondary index of tokens to hashes is maintained in the lounge to manage
;;; transactional authentication.

;;; FIXME: The current implementation of lounge is likely to become a serious
;;; bottle-neck and requires further consideration. At the very least it might
;;; be worth considering to split tokens into a separately maintained
;;; procedure, allowing for more parrallelism.
;;;
;;; My current thinking is to replace tokens with a HMAC-SHA login based
;;; system, removing their state altogether.

(define lounge
  (let ((profiles vlist-null)
        (tokens   vlist-null))
    (lambda* (lng-dir #:optional operation #:key (save? #f)
                      (token #f))
      "Procedure maintaining local state.  This is the lounge's database.  It
has side-effects and is not referentially transparent."
      (when (vlist-null? profiles)
        ;; Invocation of storage module's 'retrieve-lounge'.
        (set! profiles (compile-lounge lng-dir)))
      (cond ((not operation) (make-lounge profiles tokens)) ; lounge wanted.
            ;; Update Profile
            ((diff? operation)
             ;; Invocation of storage module's 'save-transaction'.
             ;; FIXME: use futures to write to disk as well as set!
             (when save? (write-diff operation lng-dir (current-time)))
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
            ((token? operation)
             (match (renew-tk operation tokens (current-time)) ; XXX: impure
               ((new-tks . new-tk)
                (set! tokens new-tks)
                new-tk)
               (#f #f)))
            ;; Login - impure
            (else (match (fresh-tk operation tokens profiles (current-time))
                    ((new-tks . new-tk)
                     (set! tokens new-tks)
                     new-tk)
                    (#f #f)))))))


;;;; Safe I/O Helpers

(define (store-profile diff profiles)
  "Return a new profiles vhash based on PROFILES, augmented by DIFF."
  (define (update&save what hash new-proc)
    (let ((oldp (profile-from-hash hash profiles)))
      (save hash (update-profile what (new-proc oldp) oldp)
            (vhash-delete hash profiles))))

  (match diff
    (('diff hash 'score (blobhash . (? boolean? result)))
     (update&save 'scorecard hash
                  (lambda (oldp)
                    (update-scorecard (profile-scorecard oldp) blobhash result))))
    (('diff hash 'score _)
     (error "store-profile -- invalid score value"))
    (('diff hash 'meta (name password lounge library oldhash))
     (modify-meta hash name password lounge library oldhash profiles))
    (('diff hash 'meta ())
     (cons #f (vhash-delete hash profiles)))
    (('diff hash 'meta _)
     (error "store-profile -- invalid meta value"))
    (('diff hash 'active-modules
            ((? (lambda (x) (match x
                              ((((? symbol?)) . (? blobhash?)) #t)
                              (_ #f)))
                new) ...))
     (update&save 'active-modules hash
                  (compose (cut modify-actives <> new)
                           profile-active-modules)))
    ;; FIXME: Quick 'n dirty active-mods de-activation: will
    ;; not clean the scorecard, which should happen!
    (('diff hash 'active-modules
            ('negate (? (lambda (x) (match x
                                      ((((? symbol?) . (? blobhash?))) #t)
                                      (_ #f)))
                        new) ...))
     (update&save 'active-modules hash
                  (compose (cut modify-actives <> new #t)
                           profile-active-modules)))
    (('diff hash 'active-modules _)
     (error "store-profile -- invalid active-modules"))
    (('diff hash 'hashmap hashmaps)
     (update&save 'scorecard hash
                  (lambda (oldp)
                    (add-blobs (fold append '() (map hashmap->blobs hashmaps))
                               (profile-scorecard oldp)))))
    (_ (error "store-profile -- invalid field."))))

(define (save hash profile profiles)
  "Return a new profiles vhash based on PROFILES, augmented by HASH and
PROFILE."
  (cons profile (vhash-cons hash profile profiles)))

(define (modify-meta hash name password lounge lib oldhash profiles)
  "Return a new profiles vhash, based on PROFILES, augmented by the new
profile resulting from processing the profile-hash HASH, NAME, PASSWORD,
LOUNGE, LIB and the profile's OLDHASH."
  (define (update&save what new hash)
    (save hash (update-profile what new (profile-from-hash hash profiles))
          (vhash-delete oldhash profiles)))
  (cond ((and name password lounge lib) ; Registration
         (save hash (make-bare-profile name lounge lib) profiles))
        ((and lounge oldhash) (update&save 'prof-server lounge hash))
        ((and lib oldhash)    (update&save 'mod-server lib hash))
        ((and name oldhash)   (update&save 'name name hash))
        ((and password oldhash)
         ;; Password is just stored as part of profile storage hash
         (save hash (profile-from-hash oldhash profiles)
               (vhash-delete oldhash profiles)))
        (else (error "modify-meta -- invalid values."))))

;;; '(active ...) '(active ...) boolean? -> '(active ...)
;;; where active := '(serialized-lexp . shallow-hash)
(define* (modify-actives old-actives new-candidates #:optional impair?)
  "Return a new alist based on OLD-ACTIVES augmented or impaired by
NEW-CANDIDATES, depending on the optional argument AUGMENT?."
  (fold (lambda (new actives)
          (match new
            ((lxp . shallow)
             (if impair?
                 (alist-delete lxp actives)
                 (if (assoc lxp actives)
                     actives
                     (cons new actives))))))
        old-actives
        new-candidates))


;;;; Token Operations

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
  "Return a transactional token generated from HASH and TIME. This procedure
is not referentially transparent."
(random (* 2 time)
        (seed->random-state
         (string-append hash
                        (number->string time)))))


;;;;; Profile Management
;;; Define the functions used to provide the functionality defined above.

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

;;; <profile> -> '(lexp dag-hash shallow-hash counter) | #f
(define (fetch-next-challenge-details profile)
  "A replacement for `fetch-next-hash-counter-pair', which returns, on
success, the crownset's lexp, the discipline's dag-hash, as well as the
rootset's shallow-hash and counter."
  (define (find-challenge hashes)
    (match (most-urgent hashes)
      (($ <blob> shallow par children s counter p e base-lxp dag)
       (if (null? children)
           `(,base-lxp ,dag ,shallow ,counter)
           (find-challenge children)))))
  (define (most-urgent hashes)
    (fold (lambda (current winning)
            (if (lower-score? winning current) winning current))
          (make-dummy-blob)
          (map (cute find-blob <> (profile-scorecard profile)) hashes)))

  (match (find-challenge (map cdr (profile-active-modules profile)))
    ((? dummy-blob?)   #f)
    ((? list? details) details)))

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
;; Scorecards are essentially a flat (hash?) table, associating a given
;; blobhash with the corresponding blob.
;;
;; Each blob contains the blobhash, the parent's blobhash (if applicable, else
;; #f), the children's blobhashes (if applicable, else #f), the score
;; associated with the blobhash and a counter, indicating how many challenges
;; have been provided for this blobhash.
;;
;; In this manner, locating any specific blobhash can be done in constant
;; time.
;;
;; A profile's active-modules field provides a list of all known 'crown
;; blobhashes', i.e. blobhashes that do not themselves contain parents. In
;; this way, when searching for the lowest scoring blobhash we can search the
;; 'crown-blobhashes' scores for the lowest scoring crown-blobhash, and from
;; there search each child's score until we reach the lowest-scoring
;; 'root-blobhash', i.e. the blobhash that has no children — this is the
;; blob-hash that should be provided to mod-server to generate the next
;; challenge.
;;
;; This search will take x * n score queries (constant time, see above), where
;; n is the number of blobhashes at any level (crown-blobhashes and then each
;; set of children), and x is the number of levels along this path.  (This
;; search is carried out by (fetch-next-blobhash)).

(define (hashmap->blobs hashmap)
  "Return a list of blobs, by converting each hashtree in HASHMAP to blobs."
  (match hashmap
    ((base-lxp dag-hash hashtree)
     ;; This algorithm is not great: we have massive data duplication. It's
     ;; written in this form to remind me of this.
     (hashtree->blobs base-lxp dag-hash hashtree))))

(define* (hashtree->blobs base-lxp dag-hash hashtree #:optional (parents '()))
  "Return a list of blobs by converting HASHTREE into blobs recursively"
  ;; FIXME: this procedure is currently not tail-recursive. It is also an
  ;; expensive operation in general and would benefit from being
  ;; re-factored. A lot.
  (define (qblob name children properties)
    (make-blob name parents children 0 0 properties '() base-lxp dag-hash))
  (define (children subtrees) (map caar subtrees))
  (hashtree-apply hashtree
                  (lambda (hash properties subtrees) ; branch proc
                    (cons (qblob hash (children subtrees) properties)
                          (flatten
                           (map (cute hashtree->blobs base-lxp dag-hash <>
                                      (list hash))
                                subtrees))))
                  (lambda (hash properties) ; leave proc
                    (list (qblob hash '() properties)))
                  (const #f)))            ; error proc

(define (hashtree-apply hashtree branch-proc leaf-proc error-proc)
  "If HASHTREE is a hashtree, apply either BRANCH-PROC or LEAF-PROC to it,
depending on whether it is a leaf or a branch.  If not, apply ERROR-PROC to
HASHTREE."
  (match hashtree
    (((hash . properties) subtrees)
     (branch-proc hash properties subtrees))
    (((hash . properties))
     (leaf-proc hash properties))
    (_ (error-proc))))

;;; lounge-store.scm ends here
