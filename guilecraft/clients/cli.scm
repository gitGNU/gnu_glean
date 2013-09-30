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
;;; Provide a command line client for Guilecraft.
;;;
;;;; Code:

(define-module (guilecraft clients cli)
  #:use-module (guilecraft clients min)
  #:export (cli-client))

(define (cli-client)
  (catch #t

    (lambda ()
      (call/startup client))

    handler))

(define (handler key . args)

  (define (exchange-failure? key)
    (eqv? 'exchange-error key))
  (define (server-down? key)
    (eqv? 'alive? key))
  (define (protocol-failure? key)
    (eqv? 'register-rtds key))
  (define (quit? key)
    (eqv? 'quit key))

  (cond ((quit? key)
	 (begin 
	   (simple-format #t "Thank you for playing!")
	   (newline)
	   (exit 0))
	 (server-down? key)
	 (begin
	   (simple-format #t "The server seems to be down.")
	   (newline)))
	((protocol-failure? key)
	 (begin
	   (simple-format #t "Problem registering the record types.")
	   (newline)))
	((exchange-failure? key)
	 (begin
	   (simple-format #t "Problem communicating with the
server. Server returned ~a." (object->string args))
	   (newline)))
	(else
	 (begin
	   (simple-format #t "Error: ~a" (object->string
					  key))
	   (newline)
	   (simple-format #t "~a" (object->string args))))))

(define (client)
  (define (menu msg)
    (cond ((null? (get-profile))
	   (setup))
	  ((eq? msg 'profile)
	   (begin 
	     (simple-format
	      #t 
	      "Your current profile statistics are: ~S"
	      (object->string (get-profile)))
	     (newline)
	     (menu #t)))
	  ((eq? msg 'quit)
	   (throw 'quit))
	  (else (quiz))))

  (define (setup)
    (let ((plist (rq-profiles)))
      (simple-format #t 
		     "Please enter the name of the profile you
would like to use:")
      (newline)
      (for-each (lambda (username)
		  (simple-format #t "* ~s" username)
		  (newline))
		(resolve-usernames plist))
      (let ((username (begin
			(simple-format #t "Please enter your username: ")
			(read))))
	(begin
	  (write username)
	  (newline)
	  (set-profile! (rq-auth (resolve-id username plist)))
	  #t))))

  ;; pose a problem, evaluate the answer.
  (define (quiz)
    ;; request problem, update state with new profile, pose problem
    (let ((chall-list (rq-chall (get-profile))))
      (set-profile! (car chall-list))
      (simple-format #t "Problem: ~S"
		     (cadr chall-list))
      (newline))
    ;; get player answer
    (let ((answer (read)))
      ;; check player answer for ui commands
      (cond ((or (eq? answer (quote :q))
		 (eq? answer (quote :quit)))
	     (menu 'quit))
	    ((or (eq? answer (quote :p))
		 (eq? answer (quote :profile)))
	     (menu 'profile))
	    ;; request evaluation, update state with new profile, return #t
	    (else
	     (let ((eval-list (rq-eval (get-profile) answer)))
	       (set-profile! (car eval-list))
	       (simple-format #t "Evaluation: ~S"
			      (cadr eval-list))
	       (newline))
	     #t))))

  (while #t
    (menu #t)))
