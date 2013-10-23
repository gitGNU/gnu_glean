;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

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
  #:use-module (guilecraft data-types requests)
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
	    
	    call/exchange
	    rq-auth
	    rq-chall
	    rq-eval
	    rq-profiles
	    ))

;;;; Client Support Functions

;;; Definitions in this section generally have little to do with
;;; server communication (the alive? request in (call/startup) is the
;;; only request mad to the server).  Instead, these functions provide
;;; features that will need to be implement by Guilecraft clients in
;;; any case.

(define (call/startup client-handler)
  "Perform routine checks, and initial setup.  If all is well, call
client-handler a thunk, which should implement the actual client
functionality."
  (if (alive?)
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

(define (rq-profiles)
  "Return an association list of profile names and profile ids
registered with the server."
  (call/exchange profs-list profs-rs? profs-rq))

(define (rq-auth id)
  "Return a profile if authentication is successful."
  (call/exchange auth-rs-profile auth-rs? auth-rq id))

;; The documentation for cut mentions that the order of evaluation for
;; cut and cute are unspecified.  I have introduced a test to warn us
;; if they are ever not evaluated in the correct order, rather than
;; dealing with this problem properly for now.
(define (rq-chall profile)
  "Return a new profile and challenge."
  (let ((result (call/exchange (list
				chall-rs-profile
				chall-rs-challenge)
			       chall-rs?
			       chall-rq profile)))
    (if (not (profile? (car result)))
	(throw 'problem-with-cut result)
	result)))
;; See comments for rq-chall re: cut(e)
(define (rq-eval profile answer)
  "Return a new profile and the evaluation result."
  (let ((result (call/exchange (list
				eval-rs-profile
				eval-rs-result)
			       eval-rs?
			       eval-rq profile answer)))
    (if (not (profile? (car result)))
	(throw 'problem-with-cut result)
	result)))

(define (call/exchange proc predicate rq . args)
  "Return the result of applying PROC to the server response to the
server request RQ with ARGS, after testing the response with
PREDICATE.  If PREDICATE returns false, raise an error.  If PROC is a
list, return a list containing the result of applying each proc in
list to the server response."
  (let ((result (rs-content (exchange (request (apply rq args))))))
    (if (predicate result)
	(if (list? proc)
	    (map (cut <> result) proc)
	    (proc result))
	(throw 'exchange-error result))))
