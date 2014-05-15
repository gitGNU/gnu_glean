;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

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

(define-module (guilecraft lounge-store)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft monads)
  #:use-module (guilecraft utils)
  #:use-module (ice-9 ftw)
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
            pdiff
            pdiff-profile
            lounge-monad
            token?

            login
            authenticate
            fetch-lounge
            update-lounge
            register-profile
            modify-profile
            delete-profile
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

(define-record-type <pdiff>
  (mecha-pdiff hash profile field value previous)
  pdiff?
  (hash      pdiff-hash)
  (profile   pdiff-profile)
  (field     pdiff-field)
  (value     pdiff-value)
  (previous  pdiff-previous))
(define* (pdiff hash profile field
                #:optional (value #f) (previous #f))
  (mecha-pdiff hash profile field value previous))

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
  (stateful (list value) state))

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
      (format #t "Stateful: ~a\n" new-stateful)
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
  (lambda (lng-dir)
    (let* ((tk  (lounge lng-dir hash))
           (ret (if tk tk (nothing 'unknown-user '()))))
      (statef ret lng-dir))))

(define (authenticate token)
  (lambda (lng-dir)
    (let* ((tk  (lounge lng-dir token))
           (ret (if tk tk (nothing 'invalid-token (list token)))))
      (statef ret lng-dir))))

(define (scorecard-next new-tk lng)
  (lambda (lng-dir)
    (let* ((profile (profile-from-token new-tk lng))
           (diff (missing-blobs profile)))
      (cond ((null? (profile-active-modules profile))
             (statef #f lng-dir))       ; no active-modules yet
            ;; If scorecard & active modules are in sync, proceed…
            ((null? diff)
             (statef (fetch-next-hash-counter-pair profile) lng-dir))
            ;; else force sync.
            (else
             (statef diff lng-dir))))))

(define (scorecard-diff new-tk result lng)
  (lambda (lng-dir)
    (let* ((profile (profile-from-token new-tk lng))
           (scores  (update-scorecard
                     (profile-scorecard profile)
                     (car (fetch-next-hash-counter-pair profile))
                     result)))
      (statef (pdiff (profile-hash (profile-name profile) "")
                     profile 'rescore scores)
              lng-dir))))

(define (register-profile name password lng-port lib-port lounge)
  (lambda (lng-dir)
    (if (check-name name (lounge-profiles lounge))
        (statef (nothing 'username-taken (list name)))
        (statef (pdiff (profile-hash name password)
                       (make-bare-profile name lng-port lib-port)
                       'register)
                lng-dir))))

(define (modify-profile new-tk field value lng)
  (lambda (lng-dir)
    (catch 'modify
      (lambda ()
        (statef (modify new-tk field value lng) lng-dir))
      (lambda (k v)
        (statef (nothing (car v) (cdr v)))))))

(define (delete-profile token lounge)
  (lambda (lng-dir)
    (let ((hash (cdr (vhash-assoc token (lounge-tokens lounge)))))
      (statef (pdiff hash #f 'delete) lng-dir))))

(define (fetch-lounge)
  (lambda (lng-dir)
    (statef (lounge lng-dir) lng-dir)))

(define (update-lounge diff)
  (lambda (lng-dir)
    (statef (lounge lng-dir diff) lng-dir)))

;;;;; I/O Lounge Store Operations
;; A lounge is a database of all known profiles stored in a vhash
;; table. A secondary index of tokens to hashes is maintained in the
;; lounge to manage transactional authentication.

(define lounge
  (let ((profiles vlist-null)
        (tokens   vlist-null))
    (lambda*
     (lng-dir #:optional operation)
     (if (vlist-null? profiles)
         (set! profiles (compile-lounge lng-dir)))
     (cond ((not operation) (make-lounge profiles tokens))
           ((eqv? 'tks operation) tokens)
           ((pdiff? operation)
            ;; use futures to write to disk as well as set!
            (write-pdiff lng-dir operation)
            (set! profiles (store-profile (pdiff-hash     operation)
                                          (pdiff-profile  operation)
                                          (pdiff-field    operation)
                                          (pdiff-value    operation)
                                          (pdiff-previous operation)
                                          profiles)))
           ((token?        operation)
            (let ((tk-pair (renew-tk operation
                                     tokens
                                     (current-time))))
              (if tk-pair
                  (begin
                    (set! tokens (car tk-pair))
                    (cdr tk-pair))
                  #f)))
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

(define (compile-lounge lng-dir)
  ;; Perform ftw etc.
  vlist-null)

(define (write-pdiff lng-dir pdiff)
  "Write PDIFF to LNG-DIR according to contents of PDIFF. The return
value is unspecified."
  (let ((field (pdiff-field pdiff)))
    (cond ((eqv? field 'register)
           '(save (pdiff-hash pdiff) (pdiff-profile pdiff)))
          ((eqv? field 'delete)
           '(delete (pdiff-hash pdiff)))
          ((eqv? field 'rescore)
           '(save-score (pdiff-hash pdiff)))
          ((or (eqv? field 'name)
               (eqv? field 'password))
           '(link-profs-and-save (pdiff-hash pdiff) (pdiff-field pdiff)
                                 (pdiff-value pdiff)
                                 (pdiff-field previous)))
          (else '(save-field (pdiff-hash pdiff) (pdiff-field pdiff)
                             (pdiff-value pdiff))))
    'undefined))

;;;;; Safe I/O Helpers
(define (store-profile hash profile field value previous profiles)
  (cond ((eqv? field 'name)
         (vhash-cons hash profile
                     (vhash-delete
                      (profile-hash previous "")
                      profiles)))
        ((eqv? field 'password)
         (vhash-cons hash profile
                     (vhash-delete
                      (profile-hash (profile-name profile) previous)
                      profiles)))
        ((eqv? field 'delete)
         (vhash-delete hash profiles))
        (else (vhash-cons hash profile (vhash-delete hash profiles)))))

(define (fresh-tk hash tokens profiles time)
  (if (vhash-assoc hash profiles)
      (let ((tk  (make-token hash time))
            (tks (clear-tokens hash tokens time)))
        (cons (vhash-cons tk (tk-entry hash (mk-time time)) tks) tk))
      #f))
(define (renew-tk token tokens time)
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
(define (profile-from-token token lounge)
  (let* ((hash    (vhash-assoc token (lounge-tokens lounge)))
         (profile (if hash
                      (vhash-assoc (tk-hash (cdr hash))
                                   (lounge-profiles lounge))
                      hash)))
    (if profile (cdr profile) profile)))

(define (token? obj) (number? obj))

(define (clear-tokens hash tokens time)
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

(define (modify new-tk field value lng)
  (let ((profile (profile-from-token new-tk lng)))
    (define (make-active-modules active-modules)
      (define (parse-active-modules)
        (fold (lambda (current previous)
                (if (and previous
                         (pair? current)
                         (blobhash? (car current))  ; minhash
                         (blobhash? (cdr current))) ; fullhash
                    #t #f))
              #t active-modules))
      (cond ((not (list? active-modules))
             (throw 'modify `(invalid-active-modules ,active-modules)))
            ((not (parse-active-modules))
             (throw 'modify `(invalid-active-module ,active-modules)))
            ;; Structure OK: we can update the active-modules.
            (else (append active-modules
                          (profile-active-modules profile)))))
    (define (make-scorecart hashmap)
      (let ((blobs (hashmap->blobs hashmap))
            (scorecard (profile-scorecard profile)))
        ;; Add new blobs to scorecard-data
        (add-blobs blobs scorecard)))
    (define (make-name name)
      (cond ((not (string? name))
             (throw 'modify 'invalid-name))
            ((check-name name (lounge-profiles lng))
             (throw 'modify 'name-already-in-use))
            (else name)))
    (define (make-prof-server server)
      (cond ((not (string? server))
             (throw 'modify 'invalid-prof-server))
            (else server)))
    (define (make-mod-server server)
      (cond ((not (string? server))
             (throw 'modify 'invalid-mod-server))
            (else server)))
    (define (make-field field-name field-accessor constructor)
      (cond ((eqv? field field-name)
             (constructor value))
            (else (field-accessor profile))))
    (let* ((name (make-field 'name profile-name make-name))
           ;; Id is automatically created and depends on name.
           (id (create-profile-id name))
           (prof-server (make-field 'prof-server profile-prof-server
                                    make-prof-server))
           (mod-server (make-field 'mod-server profile-mod-server
                                   make-mod-server))
           (active-modules
            (make-field 'active-modules profile-active-modules
                        make-active-modules))
           (scorecard (make-field 'scorecard profile-scorecard
                                  make-scorecart))
           (new-profile (make-profile name id prof-server mod-server
                                      active-modules scorecard)))
      ;; cycle through field names, when FIELD matches field name, make
      ;; value the value the new FIELD
      (pdiff (profile-hash name "") new-profile field value))))

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
        (throw 'no-blob-found-in-scorecard!)
        (cons (blob-hash selected-blob) (blob-counter selected-blob)))))

(define (check-name name profiles)
  (vhash-fold (lambda (hash profile found)
                (if (not found)
                    (string=? name (profile-name profile))
                    found))
              #f profiles))


;;;;;; On Scorecards and Blobhashes
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


(define (hashmap->blobs hashtree)
  "Return a flat list of blobs on the basis of HASHMAP."
  (hashmap hashtree '()))

(define (hashmap subtree parents)
  "Subtree is a list of 2 element lists, the car of which is an
identifier, the cadr of which is a further subtree. If the cadr is '()
then we have reached the bottom."
  (define (children subtree)
    "The children in a subtree are the first descendants in it, i.e. the
car of the 2 elem pairs."
    (map car subtree))
  (define (qblob name parents children)
    (make-blob name parents children 0 0))

  ;; FIXME: use ice9 match
  (fold append
        '()
        (map (lambda (2-els)
               (if (null? (cdr 2-els))
                   (cons (qblob (car 2-els)
                                parents
                                '())
                         '())
                   (cons (qblob (car 2-els)
                                parents
                                (children (cadr 2-els)))
                         (hashmap (cadr 2-els) (list (car 2-els))))))
             subtree)))

(define hmap '((dyucztliu5g4llkslb3bo4w4rvq3pmff2ib2vqxlcssldcy4jcpa
                ((x4c2x5v2tx2qyjupiicu6ljsteqyjbl7hwgonbyrkdn22kcaateq
                  ((uxxrthejznsxxld3bnd6vzitpmoja77rgeivqwfqslv4ulg2yazq)
                   (b24le6auq4tflnp7crsdzvjqc3igq75utsinobllfbhe2bobckza)
                   (q4kggp2holmxoak6afgxbrdvtcnhxb2nfybkh2cfmofyqdfdvmca)
                   (l2pikntbm3xhhve3wnolsorusogbmksaq7mtsjsyuhzu3mnvde2q)
                   (#{6npfg64yjplkocvqdpijqd2zdlmwdalczlq7ntnhw3ae3yj4xvbq}#)
                   (ez3qyedjcps7kj5wa3p56gw5wcf34u6sxe7albfkuqqsvxzntsha)
                   (cco3wvdmcv5anndzwr2i3qnj2pfwnii4roilsyuwcknl3rgbnw5a)
                   (aopve5hjnmmhcowdfc4afxzxrmhqgu4wrhy45hnyu427raitvczq)))
                 (rbpmcgmzsczyocm6a5e6qzryf4773syulfhgm5lurkllam5f3yda
                  ((eqydayggtxa3w6iw2swrcvy2qiqhhw6xzwnaudrkhmteuvvb2tyq)))))))
