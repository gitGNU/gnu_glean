;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;;; Client Base Library

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

;;;; Commentary:
;;;
;;; Provide the common interfaces likely to be used by all other
;;; client implementations.
;;;
;;;; Code:

(define-module (guilecraft clients min)
  #:use-module (guilecraft config)
  #:use-module (guilecraft comtools)
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft data-types profile-requests)
  #:use-module (guilecraft data-types module-requests)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (srfi srfi-26)
  #:export (
	    call/startup
	    get-profile
	    set-profile!
	    resolve-usernames
	    resolve-id
	    
	    register-player
	    authenticate-player

            fetch-id-hash-pairs
            push-actives-modules

            fetch-hashmap
            push-scorecard

            fetch-challenge-id
            fetch-challenge

            fetch-evaluation
            push-evaluation
            ))

;;;; Client Support Functions

;;; Definitions in this section generally have little to do with
;;; server communication (the alive? request in (call/startup) is the
;;; only request made to the server).  Instead, these functions provide
;;; features that will need to be implement by Guilecraft clients in
;;; any case.

(define (call/startup client-handler)
  "Perform routine checks, and initial setup.  If all is well, call
client-handler a thunk, which should implement the actual client
functionality."
  (if (alive? %profile-socket-file%)
      (client-handler)
      (throw 'alive?)))

(define (resolve-id username usernames)
  (define (ri remaining)
    (cond ((null? remaining)
	   #f)
	  ((equal? username (car (car remaining)))
	   (cdr (car remaining)))
	  (else (ri (cdr remaining)))))
  (ri usernames))

(define (resolve-usernames usernames)
  (map car usernames))

(define (get-profile)
  "Retrieve the latest profile from the local state manager."
  (state-manager 'get #:key 'profile))
(define (set-profile! new-profile)
  "Destructively modify the profile entry in the local state manager."
  (state-manager 'put #:key 'profile #:value new-profile))

(define state-manager
  (let ((state '((profile . ()))))
    (lambda* (message #:key (key 'profile) value)
      "Generic interface used to manage data for the client.
Currently only contains the player profile."

      (define (set-state! key new-value)
	(set! state (assq-set! state 
			       key
			       new-value)))
      (define (get-state key)
	(assq-ref state key))
      
      (cond ((eqv? message 'put)
	     (set-state! key value))
	    ((eqv? message 'get)
	     (get-state key))))))

;;;; Communication Wrappers

;;; This section contains wrappers and convenience procedures for
;;; clients to communicate with guilecraft servers.  The approach has
;;; been to provide a powerful general procedure (call/exchange),
;;; which is capable of returning very precise data sets from the
;;; server, and to then provide wrappers for individual requests and
;;; their responses that use call/exchange to provide data to the
;;; client program.

(define (register-player name profile-server module-server)
  "Return an auths upon successful profile registration."
  (call/exchange auths? regq profile-server
                 name profile-server module-server))

(define (authenticate-player name profile-server)
  "Return an auths or ERROR."
  (call/exchange auths? authq profile-server
                 name))

(define (add-active-modules token module-ids
                            profile-server module-server)
  ;; Get sethash pairs
  (fetch-id-hash-pairs module-ids module-server)
  ;; Update profile active modules
  (push-actives-modules token id-hash-pairs profile-server)
  ;; Get hashmaps if necessary
  (fetch-hashmap token crownsets module-server)
  ;; Update profile scorecards with hashmaps
  (push-scorecard token hashmap profile-server))

(define (next-challenge token profile-server module-server)
  ;; Get next challenge blobhash/counter
  (fetch-challenge-id token profile-server)
  ;; Get next problem
  (fetch-challenge blobhash blobcounter module-server))

(define (submit-answer token answer profile-server module-server)
  ;; Evaluate answer
  (fetch-evaluation blobhash blobcounter answer module-server)
  ;; Update profile scorecard
  (push-evaluation token evaluation profile-server))

(define (fetch-challenge-id token profile-server)
  "Return chauths or ERROR."
  (call/exchange chauths? chauthq profile-server token))

(define (fetch-challenge blobhash blobcounter module-server)
  "Return challs or ERROR."
  (call/exchange challs? challq module-server blobhash blobcounter))

(define (fetch-evaluation blobhash blobcounter answer module-server)
  "Return evals or ERROR."
  (call/exchange evals? evals module-server
                 blobhash blobcounter answer))

(define (push-evaluation token evaluation profile-server)
  "Return evals or ERROR."
  (call/exchange auths? evauthq profile-server
                 token evaluation))

(define (fetch-hashmap crownsets module-server)
  "Return hashmaps or ERROR."
  (call/exchange hashmaps? hashmapq module-server crownsets))

(define (fetch-id-hash-pairs set-ids module-server)
  "Return a sethashess or ERROR."
  (call/exchange sethashess? sethashesq module-server set-ids))

(define (push-scorecard token hashmap profile-server)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (push-data token 'scorecard hashmap profile-server))

(define (push-actives-modules token id-hash-pairs profile-server)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (push-data token 'active-modules id-hash-pairs profile-server))

(define (push-data token type data profile-server)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (let ((result (lesser-call/exchange set!q profile-server
                                      token type data)))
    (if (or (auths? result)
            (set!s? result))
        result
        (throw 'exchange-error result))))

(define (call/exchange predicate rq target . args)
  "Return the expected request of performing an RQ with ARGS on TARGET
as validated by PREDICATE. Raise an error if the response is not
expected."
  (let ((result (apply lesser-call/exchange rq target args)))
    (if (predicate result)
        result
        (throw 'exchange-error result))))

(define (lesser-call/exchange rq target . args)
  "Return the request of performing an RQ with ARGS on TARGET. Raise
an error if the response is not expected."
  (rs-content (exchange (request (apply rq args)) target)))

(define (call/exchange1 proc predicate rq target . args)
  "Return the result of applying PROC to the server response to the
server request RQ with ARGS, after testing the response with
PREDICATE.  If PREDICATE returns false, raise an error.  If PROC is a
list, return a list containing the result of applying each proc in
list to the server response."
  (let ((result (rs-content (exchange (request (apply rq args)) target))))
    (if (predicate result)
	(if (list? proc)
	    (map (cut <> result) proc)
	    (proc result))
	(throw 'exchange-error result))))
