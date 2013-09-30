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
;;; Provides a web user interface. It can be published to other
;;; network participants through a reverse proxy setup with other web
;;; servers (e.g. Apache, Nginx).
;;;
;;;; Code:

(define-module (guilecraft clients web)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (sxml simple)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (guilecraft clients min)
  #:export (web-client))

(define (web-client)
  (catch #t

    (lambda ()
      (call/startup client))

    handler))

(define (handler key . args)
  (define (server-down? key)
    (eqv? 'alive? key))
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
	(else
	 (begin
	   (simple-format #t "Error: ~a" (object->string
					  key))
	   (newline)
	   (simple-format #t "~a" (object->string args))))))

(define (call/response body title)
  (respond
   `((h1 guilecraft)
     (ul
      (li (a (@ (href . "/")
		(title . "Home"))
	     "Home"))
      (li (a (@ (href . "/profile/")
		(title . "View your profile"))
	     "View your profile")))
     ,body)
   #:title title))

(define (client)

  (define (dispatcher request body)
    (catch #t
      (lambda ()
	(let ((query (request-path-components request)))
	  (cond ((or (equal? query '("home"))
		     (equal? query '()))
		 (home request body))
		((equal? query '("login"))
		 (login request body))
		((equal? query '("challenge"))
		 (challenge request body))
		((equal? query '("problem"))
		 (problem request body))
		((equal? query '("eval"))
		 (eval request body))
		(else (throw 'unknown-path request body)))))

      (lambda (key . args)

	(define (exchange-failure? key)
	  (eqv? 'exchange-error key))
	(define (server-down? key)
	  (eqv? 'alive? key))
	(define (protocol-failure? key)
	  (eqv? 'register-rtds key))
	(define (quit? key)
	  (eqv? 'quit key))

	(cond ((quit? key)
	       (call/response
		`((p "Thank you for playing!"))
		"Goodbye!"))
	      ((server-down? key)
	       (call/response
		`((p "The server seems to be down."))
		"Server Down"))
	      ((protocol-failure? key)
	       (call/response
		`((p "Problem registering the record types."))
		"Protocol Failure"))
	      ((exchange-failure? key)
	       (call/response
		`((p (string-append "Problem communicating with the
server. Server returned. " ,(object->string args))))
		"Exchange Failure"))
	      (else
	       (call/response
		`((p (string-append "Error: " ,(object->string
					       key)))
		  (p ,(object->string args)))
		"Generic Error"))))))

  (run-server dispatcher))

(define (templatize title body)
  `(html (head (title ,title))
	 (body ,@body)))

(define* (respond #:optional body #:key
		  (status 200)
		  (title "Hello hello!")
		  (doctype "<!DOCTYPE html>\n")
		  (content-type-params '((charset . "utf-8")))
		  (content-type 'text/html)
		  (extra-headers '())
		  (sxml
		   (and body
			(templatize
			 title
			 body))))
  (values (build-response
	   #:code status
	   #:headers `((content-type
			. (,content-type ,@content-type-params))
		       ,@extra-headers))
	  (lambda (port)
	    (if sxml
		(begin
		  (if doctype (display doctype port))
		  (sxml->xml sxml port))))))

(define (home request body)
  (call/response
   `((h1 "Play. Guilecraft. Now!")
     (div
      (@ (class . "left"))
      (p "Guilecraft is functional package management for the
brain.")
      (p "Currently the following profiles are available for use:")
      (ul ,@(map
	     (lambda (username)
	       `(li ,username))
	     (resolve-usernames (rq-profiles)))))
     (div
      (@ (class . "right"))
      (form (@ (action . "/challenge/")
	       (method . "post"))
	    (fieldset
	     (label (@ (for . "username"))
		    "Enter your username:")
	     (input (@ (type . "text")
		       (id . "username")
		       (name . "username")
		       (value . "username")))
	     (input (@ (type . "submit")))))))
   "Welcome to Guilecraft"))

(define (login request body)
  (respond
   `((h1 "Welcome!")
     (p "Please enter username and password:")
     (form (@ (action . "/challenge/") (method . "post"))
      (label (@ (for . "username")) "Username")
      (input (@ (type . "text") (id . "username")))
      (label (@ (for . "password")) "Password")
      (input (@ (type . "text") (id . "password")))
      (input (@ (type . "submit")))))))

(define (challenge request body)
  (let ((body-string (utf8->string body)))

    (define (challenge-form)
      (let ((chall-list (rq-chall (get-profile))))
	(set-profile! (car chall-list))
	`((p ,(cadr chall-list))
	  (form (@ (action . "/challenge/")
		   (method . "post"))
		(fieldset
		 (label (@ (for . "answer"))
			"Entery your answer:")
		 (input (@ (type . "text")
			   (id . "answer")
			   (name . "answer")
			   (value . "your answer")))
		 (input (@ (type . "submit"))))))))

    (call/response
     `((h1 "Your challenge")
       ,(cond ((string-prefix? "username=" body-string)
	       (let ((username (string-drop body-string (string-length
							 "username="))))
		 (set-profile! (rq-auth (resolve-id username
						    (rq-profiles)))))
	       (challenge-form))
	      ((string-prefix? "answer=" body-string)
	       `((p ,(string-append "answer body: " body-string))
		 ,(let* ((answer (string-drop body-string (string-length
							   "answer=")))
			 (eval-list (rq-eval (get-profile) answer)))
		    (set-profile! (car eval-list))
		    `((p ,(string-append "Your answer was " (object->string
							     (cadr eval-list))))
		      ,(challenge-form)))))
	      (else (challenge-form))))
     "A challenge is posed...")))

(define (diag request body)
  (call/response
   `((h1 "Diagnostics")
     (table
      (tr (th "header") (th "value"))
      ,@(map (lambda (pair)
	       `(tr (td (tt ,(with-output-to-string
			       (lambda () (display (car pair))))))
		    (td (tt ,(with-output-to-string
			       (lambda ()
				 (write (cdr pair))))))))
	     (request-headers request)))
     (p "The request body was:")
     (p ,body))
   "Diagnosis"))

(define (problem request body)
  (respond
   `((h1 "Hello Guilecraft!")
     (p "Guilecraft is a flexible and extensible quizzing system written in Guile.")
     (p "Vist /login/ to get started")
     )))

(define (eval request body)
  (respond
   `((h1 "Hello Guilecraft!")
     (p "Guilecraft is a flexible and extensible quizzing system written in Guile.")
     (p "Vist /login/ to get started")
     )))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))
