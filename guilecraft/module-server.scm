;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;;; Module Server

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
;;; The Module Server manages known guilecraft modules and
;;; module-hash-maps. It also provide challenge access and evaluation
;;; functionality.
;;;
;;; It uses the server communication framework defined in base-server,
;;; implements a module server specific dispatcher, and the logic for
;;; carrying out the functionality implied through the requests it
;;; receives.

;;;; Documentation:
;;; FIXME: Write documentation

;;; Code:

(define-module (guilecraft module-server)
  #:use-module (guilecraft base-server)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-26)
  #:use-module (guilecraft gmodule-manager)
  #:use-module (guilecraft problem-type-manager)
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft data-types module-requests)
  #:use-module (guilecraft data-types profile-requests)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft comtools)
  #:use-module (guilecraft gset-ops)
  #:use-module (guilecraft portal)
  #:use-module (guilecraft utils)
  #:use-module (guilecraft store)
  #:use-module (guilecraft gmodule-manager)
  #:use-module (rnrs)
  #:export (module-server))

;;;;; Module Server Dispatch Logic
;;;; Define the actual module server and the server-dispatcher used
;;;; by it.
(define (module-server module-socket-file)
  (store-modules)
  (the-server module-socket-file server-dispatcher))

(define (server-dispatcher request)
  "Interprets client requests, and passes additional information for
handling to request handler."

  (define (challenge-provider challenge-rq)
    (guard (err ((or (eqv? err 'no-profile)
		     (eqv? err 'no-modules)
		     (eqv? err 'invalid-token)
		     (eqv? err 'invalid-auth-server))
		 (begin (clog err)
			(neg-rs challenge-rq err)))
		((equal? err '(exchange (server system-error)))
		 (begin (clog err)
			(neg-rs challenge-rq 'offline-auth-server)))
		((eqv? err 'false-result)
		 (begin (llog err)
			(assertion-violation
			 'challenge-provider
			 "We were returned a #f chall result!"
			 err))))
	   (cond ((not (number? (challq-token challenge-rq)))
		  (raise 'invalid-token))
		 ((not (string? (challq-auth-server challenge-rq)))
		  (raise 'invalid-auth-server))
		 (else (chall-rs (new-challenge
				  (challq-token challenge-rq)
				  (challq-auth-server challenge-rq)))))))

  (define (eval-provider eval-rq)
    (guard (err ((or (eqv? err 'no-profile)
		     (eqv? err 'no-modules)
		     (eqv? err 'invalid-token)
		     (eqv? err 'invalid-auth-server)
		     (eqv? err 'invalid-answer))
		 (begin (clog err)
			(neg-rs eval-rq err)))
		((equal? err '(exchange (server system-error)))
		 (begin (clog err)
			(neg-rs eval-rq 'offline-auth-server)))
		((eqv? err 'false-result)
		 (begin (llog err)
			(assertion-violation
			 'eval-provider
			 "We were returned a #f eval result!"
			 err))))
	   (cond ((not (string? (evalq-auth-server eval-rq)))
		  (raise 'invalid-auth-server))
		 ((not (string? (evalq-answer eval-rq)))
		  (raise 'invalid-answer))
		 (else (eval-rs (eval-challenge
				 (evalq-token eval-rq)
				 (evalq-auth-server eval-rq)
				 (evalq-answer eval-rq)))))))

  (define (hashmap-provider rq)
    (guard (err (err
		 (begin (clog err)
			(neg-rs rq err))))
	   (let ((token (hashmapq-token rq))
		 (crownsets (hashmapq-crownset-ids rq))
		 (blobhashes (hashmapq-blobhashes rq)))
	     (cond ((not (list? crownsets))
		    (raise 'invalid-crownsets))
		   ((not (list? blobhashes))
		    (raise 'invalid-blobhashes))
		   (else (hashmaps token
				   (generate-hashmap crownsets
						     blobhashes)))))))

  (cond ((eof-object? request)
	 #f)
	((request? request)
	 (let ((rq (rq-content request)))
	   (gmsg #:priority 8 "server-dispatcher: rq-content:" rq)
	   (cond ((alive-rq? rq)
		  (ack-rs rq))
		 ((chall-rq? rq)
		  (challenge-provider rq))
		 ((eval-rq? rq)
		  (eval-provider rq))
		 ((hashmapq? rq)
		  (hashmap-provider rq))
		 ((hash-rq? rq)
		  (hash-provider rq))
		 ((quit-rq? rq)
		  (ack-rs rq))
		 (else (unk-rs rq)))))
	(else (unk-rs request))))

;;;;; Module Management
;;;; Define the functions used to provide the functionality defined
;;;; above.
(define (generate-hashmap crownsets blobhashes)
  "Return a hashmap, built preferably off of the list CROWNSETS,
alternatively off of the list BLOBHASHES if the former is empty. If
both are empty, raise an error."
  ;; FIXME: Not actually implemented find-crownsets. Necessary?
  (cond ((not (null? crownsets))
	 '(find-crownsets crownsets))
	((not (null? blobhashes))
	 (find-blobhashes blobhashes))
	(else (raise '(generate-hashmap empty-hashmapq)))))
(define (find-blobhashes blobhashes)
  "Return the list of set-paths, with each id converted to its hash
for all items in the list BLOBHASHES."
  ;; FIXME: do we need to convert the set-paths to hashes?
  (map lookup-hash blobhashes))
(define (new-challenge token auth-server)
  "Return a chall-rs for the player identified by TOKEN, by retrieving
the highest priority set from AUTH-SERVER."
  (let ((chauth-rs
	 (exchange (request (chauth-rq token)) auth-server)))
    (list (chauths-token chauth-rs)
	  (ptm_get-challenge
	   (fetch-problem
	    (lookup-hash (chauths-hash chauth-rs))
	    (chauths-counter chauth-rs))))))
(define (eval-challenge token auth-server answer)
  (let ((chauth-rs
	 (exchange (request (chauth-rq token)) auth-server)))
    ;;eval-answer with hash above
    ;;evauth eval result
    ;;return eval-rs
    #t
    ))
;;;;; Support Functions
(define (lookup-hash hash)
  "Return the set-path of the set identified by HASH.

Convenience procedure accessing the set-hash-store."
  (set-hash-store 'get hash))
(define (init-hash-table modules)
  "Return value is undefined. Initiate the set-hash-store with
MODULES.

Convenience procedure accessing the set-hash-store."
  (set-hash-store 'init modules))
(define (fetch-problem set-path problem-counter)
  "Return the problem identified by SET-PATH and PROBLEM-COUNTER.

SET-ID-PATH is a list of set-ids in descending order along a module
hierarchy, starting with the name of the crown set. The last set,
which contains the problem we look for, is the root
set. PROBLEM-COUNTER then identifies the problem within the root set
that we are looking for."
  (define (fetch-root-set module remaining-path)
    "Travel down REMAINING-PATH along MODULE, until we find the root
set."
    (cond ((null? (cdr remaining-path))
	   (find-next-set (car remaining-path)
		      (set-contents module)))
	  (else (fetch-root-set
		 (find-next-set (car remaining-path)
			    (set-contents module))
		 (cdr remaining-path)))))
  (define (find-next-set id sets)
    "Search SETS for that set identified by set-id ID."
    (cond ((null? sets)
	   #f)
	  ((eqv? id (set-id (car sets)))
	   (car sets))
	  (else (find-next-set id (cdr sets)))))

  (let* ((root-set
	  (fetch-root-set (gman_get-gmodule (car set-path))
			  (cdr set-path)))
	 (problems (set-contents root-set))
	 (num-of-problems (length problems)))
    (list-ref problems
	      (modulo problem-counter num-of-problems))))
;;;;; Data Storage
(define set-hash-store
  (let ((set-hashes #f))
    (lambda (msg . args)
      "Central store containing a mapping of the hashes of all known sets
to a representation of the location of that set in the module
hierarchy (these mappings are called set-paths."

      (define (set-paths modules)
	"Return a list of set-paths for all sets contained in the list
MODULES."
	(define (module-enumerator module)
	  "Return a list of set-paths by traversing the module hierarchy of
the crown set MODULE, accumulating every possible set-path from it to
any of its root sets."
	  (define (rec-sets sets)
	    "Return the set-paths to the root sets of all sets in the list
of SETS.

This does not return the crown set, which is identified in
module-enumerator."
	    (map (lambda (set)
		   (cond ((problem? (first-contents set))
			  (cons (set-id set) '()))
			 (else (cons (set-id set)
				     (rec-sets (set-contents set))))))
		 sets))

	  (cond ((null? (set-contents module)) ; error: no contents!
		 #f)
		((problem? (first-contents module)) ; crown set is root set
		 (set-id module))
		(else (map (lambda (set-chain) ; we need to traverse.
			     (cons (set-id module)
				   set-chain))
			   (rec-sets (set-contents module))))))

	(fold-left append '() (map module-enumerator (stored-modules))))

      (define (hashes set-paths)
	"Return a list of hashes for all sets identified by SET-PATHS."
	(define (hash set-path)
	  (string-hash
	   (fold-left (lambda (previous sym)
			(string-append previous "-" (symbol->string sym)))
		      (string-append (symbol->string (car set-path)))
		      (cdr set-path))))
	(map (lambda (module-set-paths)
	       (hash module-set-paths))
	     set-paths))

      (cond ((eqv? msg 'init)
	     (let* ((modules (car args))
		    (set-paths (set-paths modules))
		    (hashes (hashes set-paths)))
	       (set! set-hashes (make-eqv-hashtable (length modules)))
	       (for-each (lambda (hash set-path)
			   (hashtable-set! set-hashes
					   hash
					   set-path))
			 hashes
			 set-paths)))
	    ((eqv? msg 'get)
	     (let ((key (car args)))
	       (hashtable-ref set-hashes key #f)))
	    ((eqv? msg 'list)
	     (hashtable-entries set-hashes))))))
