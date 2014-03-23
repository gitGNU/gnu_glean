;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;;; Profile Server

;; Copyright (C) 2008, 2010, 2012 Alex Sassmannshausen

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

;;; Commentary:
;;;
;;; The Profile Server manages authentication, user profiles (both
;;; in-game and profile persistence) and user scorecards.
;;;
;;; It uses the server communication framework defined in base-server,
;;; implements a profile server specific dispatcher, and the logic for
;;; carrying out the functionality implied through the requests it
;;; receives.

;;;; Documentation:
;;; FIXME: Write documentation

;;; Code:

(define-module (guilecraft profile-server)
  #:use-module (guilecraft base-server)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-26)
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft data-types profile-requests)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft comtools)
  #:use-module (guilecraft data-manager)
  #:use-module (guilecraft utils)
  #:use-module (guilecraft config)
  #:use-module (rnrs)
  #:export (profile-server))

;;;;; Profile Server Dispatch Logic
;;;; Define the actual profile server and the server-dispatcher used
;;;; by it.
(define (profile-server profile-socket-file)
  "Launch a profile-server."
  (the-server profile-socket-file server-dispatcher))

(define (server-dispatcher request)
  "Interprets client requests, and passes additional information for
handling to request handler."

  (cond ((eof-object? request)
	 #f)
	((request? request)
	 (let ((rq (rq-content request)))
	   (guard (err (err
			(begin (clog err)
                               (display "Error in dispatcher")
                               (newline)
			       (negs rq err))))
		  (cond ((aliveq? rq) (acks rq))
			((quitq? rq) (acks rq))
                        ((echoq? rq)
                         (process-echoq rq))
			((chauthq? rq)
			 (process-chauthq rq))
			((evauthq? rq)
			 (process-evauthq rq))
			((authq? rq)
			 (process-authq rq))
			((set!q? rq)
			 (process-set!q rq))
			((modq? rq)
			 (process-modq rq))
			((regq? rq)
			 (process-regq rq))
			((delq? rq)
			 (process-delq rq))
			(else (unks rq))))))
	(else (unks request))))

;;;;; Server Response Creation
;;;; Functions that provide request specific parsing and response
;;;; skeletons.
(define (process-echoq rq)
  "Return an echos, containing a new token, the profile's profile
server, module server and the message contained in RQ."
  (let ((token (echoq-token rq)))
    (cond ((not (token? token))
	   (raise 'invalid-token))
	  (else
	   (let* ((new-token (authenticate token))
		  (profile (get-profile-by-token new-token)))
             (echos new-token (profile-prof-server profile)
                    (profile-mod-server profile) (echoq-message rq)))))))
(define (process-chauthq rq)
  "Return a chauths, containing a new token and the next hash/counter
pair. Return a set!s informing that scorecard and active-modules are
out of sync if they are."
  (let ((token (chauthq-token rq)))
    (cond ((not (token? token))
	   (raise 'invalid-token))
	  (else
	   (let* ((new-token (authenticate token))
		  (profile (get-profile-by-token new-token))
		  (diff (missing-blobs profile)))
	     ;; If scorecard & active modules are in sync, proceed…
	     (if (null? diff)
                 (let ((blob-pair (fetch-next-hash-counter-pair
                                   profile)))
                   (chauths new-token
                            (car blob-pair)
                            (cdr blob-pair)
                            (profile-mod-server profile)))
		 ;; else force sync.
		 (set!s new-token 'scorecard diff
			(profile-mod-server profile))))))))

(define (process-evauthq rq)
  (let ((token (evauthq-token rq))
        (result (evauthq-result rq)))
    (display "process-evauthq: start")
    (newline)
    (cond ((not (token? token))
           (raise 'invalid-token))
          ((not (boolean? result))
           (raise 'invalid-result))
          (else
           (display "process-evauthq: in else")
           (newline)
           (process-evaluation-result token result)))))

(define (process-authq rq)
  (if (string? (authq-name rq))
      ;; Auths expects 2 values: token and mod-server, so
      ;; authenticate-user will return a list.
      (apply auths
	     (authenticate-user
	      (authq-name rq)
	      ;; Password to come here.
	      ))
      (raise 'invalid-username)))
(define (process-set!q rq)
  (let ((token (set!q-token rq))
	(field (set!q-field rq))
	(value (set!q-value rq)))
    (cond ((not (symbol? field))
	   (raise 'invalid-field))
	  ((not (token? token))
	   (raise 'invalid-token))
	  (else
	   (let* ((new-token
		   (modify-profile field value
				   (get-profile-by-token
				    token) token))
		  (diff (missing-blobs (get-profile-by-token new-token))))
	     (if (null? diff)
		 (auths 
		  new-token
		  (profile-mod-server
		   (get-profile-by-token new-token)))
		 (set!s new-token 'scorecard diff)))))))
(define (process-modq rq)
  (let ((field (modq-field rq))
	(token (modq-token rq)))
    (cond ((not (symbol? field))
	   (raise 'invalid-field))
	  ((not (token? token))
	   (raise 'invalid-token))
	  (else
	   (let ((new-token (authenticate token)))
	     (mods new-token
		   (prep-modification
		    field
		    (get-profile-by-token
		     new-token))))))))
(define (process-delq rq)
  (if (token? (delq-token delq))
      (acks
       (delete-user
	(delq-token delq)))
      (raise 'invalid-token)))
(define (process-regq rq)
  (cond ((not (string? (regq-name rq)))
	 (raise 'invalid-username))
	((not (string? (regq-mod-server rq)))
	 (raise 'invalid-mod-server))
	;; Auths expects 2 values: token and mod-server, so
	;; register-user will return a list.
	(else (apply auths
		     (register-user
		      (regq-name rq)
		      ;; Password to come here.
		      (regq-prof-server rq)
		      (regq-mod-server rq))))))

;;;;; Profile Management
;;;; Define the functions used to provide the functionality defined
;;;; above.
(define (missing-blobs profile)
  "Return a list of blobs not currently contained in the
scorecard of PROFILE. Used to check whether we need to request a new
hashmap."
  (define (check-blob-maker scorecard-data)
    (lambda (results active-module)
      (if (scorecard-data 'get (cdr active-module))
	  results (cons active-module results))))

  (fold-left (check-blob-maker
	      (scorecard-data (profile-scorecard profile)))
	     '()
	     (profile-active-modules profile)))

(define (prep-modification field profile)
  "Return the value of the field in PROFILE identified by FIELD."
  (define (get-accessor rtd)
    (fold-left (lambda (index pot-field)
		 (if (number? index)
		     (if (eqv? field pot-field)
			 (record-accessor rtd index)
			 (1+ index))
		     index))
	       0
	       (vector->list (record-type-field-names rtd))))

  (if (eqv? field 'blobs)
      ((scorecard-data (profile-scorecard profile)) 'values)
      (let ((accessor (get-accessor (record-rtd profile))))
	(if (procedure? accessor)
	    (accessor profile)
	    (raise '(prep-modification unknown-field))))))

(define (modify-profile field value profile token)
  "Return a new transactional token if PROFILE's FIELD was
successfully updated with the new VALUE. Otherwise, raise an error."
  (define (make-active-modules active-modules)
    (define (parse-active-modules)
      (fold-left (lambda (previous current)
		   (if (and previous
			    (pair? current)
			    (symbol? (car current))
			    (blobhash? (cdr current)))
		       #t #f))
		 #t active-modules))
    (cond ((not (list? active-modules))
	   (raise '(modify-profile invalid-active-modules)))
	  ((not (parse-active-modules))
	   (raise `(modify-profile invalid-active-module ,active-modules)))
	  ;; Structure OK: we can update the active-modules.
	  (else active-modules)))
  (define (make-scorecart hashmap)
    (let ((blobs (hashmap->blobs hashmap))
	  (scorecard (profile-scorecard profile)))
      ;; Add new blobs to scorecard-data
      (add-blobs blobs scorecard)))
  (define (make-name name)
    (cond ((not (string? name))
	   (raise '(modify-profile invalid-name)))
	  ((check-profile name)
	   (raise '(modify-profile name-already-in-use)))
	  (else name)))
  (define (make-prof-server server)
    (cond ((not (string? server))
	   (raise '(modify-profile invalid-prof-server)))
	  ;; FIXME: Should attempt connection to server (i.e. myself).
	  (else server)))
  (define (make-mod-server server)
    (cond ((not (string? server))
	   (raise '(modify-profile invalid-mod-server)))
	  ;; FIXME: Should attempt connection to server and test.
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
    ;; value the for the new FIELD
    (if (eqv? field 'name)
	(migrate-profile (profile-name profile) new-profile token)
	(update-profile! new-profile token))))

(define (process-evaluation-result token result)
  "Return an evauthq containing TOKEN and RESULT as a confirmation
that the scorecard has been updated, or raise an error if problems are
encountered.

First, validate token. Second, match the set in the scorecard to be
updated. Third, update the set in the scorecard. Finally, return
evauthq."
  ;; First, generate new token
  ;; Second, update profile
  ;;; raise on failure
  ;;; else return auths
  (let* ((new-token (authenticate token))
         (profile (get-profile-by-token token))
         (scorecard (profile-scorecard profile))
         (blobhash (car (fetch-next-hash-counter-pair profile))))
    (begin
      (update-scorecard scorecard blobhash result)
      (auths new-token
             (profile-mod-server profile)))))

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
  "Return a flat list of blobs on the basis of HASHMAP."
  ;; hashmap takes the shape of:
  ;; '((hash ((hash ((hash ((hash ())
  ;; 			       (hash ((hash ())
  ;; 					(hash ())))
  ;; 			       (hash ())))
  ;;                     (hash ((hash ())
  ;; 		                  (hash ())))))
  ;; 	     (hash ())))
  ;;   (hash ((hash ((hash ((hash ())
  ;; 			       (hash ())))))
  ;; 	     (hash '()))))
  ;; hashpath is a single crownhash and offspring in the mapIt takes
  ;; the form of:
  ;; '(hash ((hash ((hash '())
  ;; 		      (hash '())))
  ;; 	     (hash ((hash '())))))
  ;; each node in a hash path is a pair containing the hash in
  ;; question and a list of its child hashes.

  ;; Need to flatten as each invocation of hashpath-blobs returns a
  ;; list.
  (flatten (map hashpath->blobs* hashmap)))
(define (hashpath->blobs* node)
  ;; cycle through each element in hashpath, turning every entry into
  ;; a blob in a flat results list.
  ;; (i.e. turn a nested list of hashpaths into flat list of blobs.
  (define (self node)
    (if (and (list? node)
             (not (null? node)))
        (car node)
        (raise 'node-not-list)))
  (define (children node) (cdr node))
  (define (childless? node) (null? (children node)))
  (define (current remaining-nodes) (car remaining-nodes))
  (define (siblings remaining-nodes) (cdr remaining-nodes))
  (define (no-more? remaining-nodes) (null? remaining-nodes))
  (define (traverse-path remaining-nodes parents)
    (if (no-more? remaining-nodes)
	'()				; finish list when done.
	(let ((node (current remaining-nodes)))
	  (cond ((childless? node)	; root node
		 ;; make root-blob and proceed to siblings.
		 (cons (make-blob (self node) parents '() 0 0)
		       (traverse-path (siblings remaining-nodes)
				      parents)))
		;; Not childless: must recurse on self's children and
		;; on its siblings.
		;; Currently this causes nesting, hence the need to
		;; flatten the blobs list at the end.
		(else
		 ;; make blob and...
		 (cons (make-blob (self node) parents
				  (map self (children node)) 0 0)
		       ;; recurse on children and ...
		       (cons (traverse-path (children node)
					    (list (self node)))
			     ;; recurse on siblings
			     (traverse-path (siblings remaining-nodes)
					    parents))))))))

  (flatten
   (cons (make-blob (self node) '() (map self (children node)) 0 0)
	 (traverse-path (children node) (list (self node))))))
;; Originally I thought we would need to keep track of a list of
;; crown-blobhashes, so that that list could act as the first set of
;; blobhashes to be searched below (root of the btree to be
;; searched?). I think this is no longer necessary, as we can use
;; PROFILE's active modules as a personalised list of
;; crown-blobhashes.
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
	  blob				; blob is a root-blob
	  (traverse-blob (fold-left highest-priority-blob
				    (make-dummy-blob)
				    children)))))
  (define (highest-priority-blob winning-blob current-blobhash)
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
	  (fold-left highest-priority-blob
		     (make-dummy-blob)
		     (active-module-hashes
		      (profile-active-modules profile))))))
    (cons (blob-hash selected-blob) (blob-counter selected-blob))))

(define (authenticate-user name)	; FIXME: Password to come here
  "Return a new token and the user's mod-server address to confirm
that name has been used to authenticate the player against a stored
profile.

First, validate name against the known-profiles. Second generate a
token and associate it with the user's profile. Finally Return the
new-token and the user's mod-server."
  (if (member name (name-table 'values))
      (raise '(authenticate-user user-already-authenticated))
      (let ((profile (get-profile-by-name name)))
        (if profile
            ;; Only place where adding a name to name-table is allowed
            `(,(add-to-name-table (make-token (profile-id profile))
                                  (profile-name profile))
              ,(profile-mod-server profile))
            (raise '(authenticate-user authentication-failed))))))
(define (delete-user token)
  "Return a delq containing TOKEN to confirm the profile has been
deleted. TOKEN is no longer valid. Otherwise, raise an error.

First, run authenticate-user with TOKEN. Then, raise error or delete
profile from db. Finally generate delq with TOKEN."
  (let ((new-token (authenticate token)))
    (if new-token
	(if (remove-profile (get-profile-by-token new-token))
	    (delq token)
	    (raise '(delete-user deletion-failed))))
    (raise '(delete-user 'authentication-failed))))
(define (register-user name prof-server mod-server)
  ;; Password to come as #2
  "Return a new token and the user's mod-server address to confirm
that the user has been registered, or raise an error if problems are
encountered."
  (let ((hash (name->hash name)))
    (if (and (number? hash)
	     (not (profile-table 'contains hash)))
	(authenticate-user (add-profile
			    (make-bare-profile name prof-server
					       mod-server)))
	(raise '(register-user name-taken)))))

;;;;; Operations
(define (check-profile name)
  "Return #t if NAME is a registered profile identifier. Otherwise
#f."
  (profile-table 'contains (name->hash name)))
(define (get-profile-by-name name)
  "Return the first profile stored under the hash of NAME in
profile-table."
  (let ((hash (name->hash name)))
    (if (check-profile name)
        (current-profile (profile-table 'get hash))
	(raise '(get-profile-by-name unknown-name)))))

;;;;; Profile Storage
(define (current-profile profile-group)
  "Return the current profile from PROFILE-GROUP. This is currently
the first profile in the list."
  (car profile-group))
(define (migrate-profile old-name profile token)
  "Return a new transactional token as confirmation that migrated
profile has been installed by copying the profile identified by
OLD-NAME to the new PROFILE's listing, and by deleting the old profile
association. Otherwise, raise an error."
  (let ((old-hash (name->hash old-name))
	(new-hash (name->hash (profile-name profile))))
    ;; Check old profile exists
    (if (profile-table 'contains old-hash)
	;; Check new profile does not yet exist
	(if (not (profile-table 'contains new-hash))
	    ;; Save old data, delete old profile entry, copy to new
	    ;; entry, update to latest profile.
	    (let ((profile-group (profile-table 'get old-hash)))
	      (remove-profile (get-profile-by-name old-name))
	      (profile-table 'put new-hash profile-group)
	      ;; This returns the new token.
	      (update-profile! profile token))
	    (raise '(migrate-profile new-name-exists)))
	(raise '(migrate-profile old-name-unknown)))))
(define (update-profile! profile old-token)
  "Return a new transactional token as confirmation that the latest
PROFILE has been installed for PROFILE-HASH. Otherwise, raise an
error."
  (let ((profile-hash (name->hash (profile-name profile))))
    (if (profile-table 'contains profile-hash)
	(if (profile-table 'update profile-hash 
                           (lambda (profile-group)
                             (cons profile profile-group))
                           #f)
            ;; Need to return a new token as the profile-id will have
            ;; changed from the last transaction.
            (update-name-table profile old-token)
            (raise '(update-profile! update-failure)))
	(raise '(update-profile! profile-hash-not-found)))))
(define (add-profile profile)
  "Return PROFILE as confirmation that the new profile has been
installed. Otherwise return an error."
  (let ((hash (name->hash (profile-name profile))))
    (if (not (profile-table 'contains hash))
	(if (profile-table 'put hash (list profile))
	    (profile-name profile)
	    (raise '(add-profile put-failure)))
	(raise '(add-profile profile-hash-exists)))))
(define (remove-profile profile)
  "Return PROFILE as confirmation that the new profile has been
installed. Otherwise return an error."
  (let ((hash (name->hash (profile-name profile))))
    (if (profile-table 'contains hash)
	(if (profile-table 'rem hash)
	    #t
	    (raise '(remove-profile rem-failure)))
	(raise '(remove-profile profile-hash-not-found)))))
;;   "Returns an instance of data-manager to store profiles by
;; profile-group.

;; A profile group is a list of revisions of a given profile. The unique
;; identifer is likely the symbol-hash of the id of the profile revision
;; current when the server is launched."
(define profile-table
  (data-manager (lambda (object)
		  (if (and (list? object)
			   (profile? (car object)))
		      #t
		      #f))))

;;;;; Name Table
;; Links current tokens to profile names which can then be used to
;; retrieve associated profiles.
(define name-table (data-manager string?))
(define (check-name token)
  "Return #t if TOKEN currently identifies a name in the name-table."
  (name-table 'contains token))
(define (get-profile-by-token token)
  "Return the profile from the profile-table, currently associated
with the name associated with TOKEN in the name-table."
  (get-profile-by-name (get-name-by-token token)))
(define (get-name-by-token token)
  "Return the name currently associated with TOKEN in the name-table."
  (if (name-table 'contains token)
      (name-table 'get token)
      (raise '(get-name-by-token unknown-token))))
(define (make-token profile-id)
  "Return a transactional token generated from profile-id.

Currently this is not random enough — it should include a salt."
  (let ((token
         (random
          (symbol-hash
           (symbol-append (id->symbol profile-id)
                          (string->symbol
                           (number->string
                            (current-time))))))))
  token))
(define (token? obj)
  (if (number? obj) #t #f))
(define (authenticate old-token)
  "Return a new token as proof that authentication of TOKEN
succeeded. Otherwise raise an error: '(authenticate
authentication-failed)"
  (if (check-name old-token)
      (update-name-table (get-profile-by-token old-token)
                          old-token)
      (raise '(authenticate authentication-failed))))

(define (update-name-table profile old-token)
  ;; Delete the old token
  (name-table 'rem old-token)
  ;; Return a new token
  (add-to-name-table (make-token (profile-id profile))
                     (profile-name profile)))
(define (add-to-name-table token profile-name)
  "Return TOKEN if we have been able to successfully associate TOKEN
with PROFILE-NAME in the name table. Otherwise, raise a warning.

Raise an error if TOKEN is already present in the table."
  (if (not (name-table 'contains token))
      (if (name-table 'put token profile-name)
	  token
	  (raise '(add-to-name-table put-failure)))
      ;; This should normally not happen: tokens should be random.
      (error 'add-to-name-table
	     "TOKEN is already present in name-table!"
	     token)))
