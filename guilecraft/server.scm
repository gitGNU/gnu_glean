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

;;; Commentary:
;;
;; Module to provide daemon mode, whereby guilecraft acts as a server,
;; listening on a socket for requests.
;;
;;; Code:

(define-module (guilecraft server)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-26)
  #:use-module (guilecraft known-rtd-manager)
  #:use-module (guilecraft record-index)
  #:use-module (guilecraft gprofile-manager)
  #:use-module (guilecraft gmodule-manager)
  #:use-module (guilecraft data-types requests)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft comtools)
  #:use-module (guilecraft portal)
  #:use-module (guilecraft utils)
  #:use-module (guilecraft store)
  #:export (server))

(define %gettext-domain
  "guilecraft")

(define _ (cut gettext <> %gettext-domain))
(define N_ (cut ngettext <> <> <> %gettext-domain))

(define (server guilecraft-dir)
  "Starts server: sets socket path, calls checks and then server loop."
  (define (thunk)
    (let ((path (string-append guilecraft-dir "/socket")))
      (if (prepare-port path)
	  (server-loop path))))
  (define (h key . args)
    (display args)
    (newline)
    (let ((socket (car args))
	  (client (cadr args))
	  (path (caddr args))
	  (state (cadddr args)))
      (server-quit socket client path state)))
  
  ;; Register the record types
  (register-rtds records)
  ;; Register the known modules
  (store-modules)
  ;; Register the known profiles
  (store-profiles)
  ;; start the server
  (catch #t thunk h))

(define (prepare-port path)
  "Checks whether PATH exists, and if so, offers to delete and
continue load."
  (if (not (file-exists? path))
      #t
      (begin
	(simple-format #t "Socket file exists. 
Did we not exit cleanly?

I can delete the socket file and continue launch.
Enter y to continue, or anything else to abort:")
	(newline)
	(let ((answer (read)))
	  (if (or (eq? answer 'y)
		  (eq? answer 'Y)
		  (eq? answer 'yes)
		  (eq? answer 'Yes))
	      (begin
		(delete-file path)
		#t)
	      (throw 'socket-error #f #f path 'prepare-port))))))

(define (build-out obj)
  (record->list* obj))


(define (server-loop path)
  "Set up the socket on PATH, and use loop to accept and process
client requests."

  (define (start-server)
    (let ((s (socket PF_UNIX SOCK_STREAM 0))
	  (sock-addr (make-socket-address AF_UNIX path)))

      (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
      (bind s sock-addr)
      (listen s 5)

      (gmsg #:priority 5 "start-server: Listening for clients in pid:" (getpid))
      s))
  
  (let ((s (start-server)))

    (define (loop iter)
      (gmsg #:priority 5 "loop: Now Processing Request:" iter)

      (let* ((connection (accept s))
	     (client (car connection)))

	(define (respond record)
	  (let ((resp (response (server-dispatcher record))))
	    (gmsg #:priority 7 "respond: resp:" resp)
	    (if resp
		(begin
		  (gwrite (record->list* resp) client)
		  (close client)
		  (gmsg #:priority 8 "respond: Client closed.")
		  (cond ((ack-rs? (rs-content resp))
			 (gmsg "respond: checking for quit-rq…")
			 (if (quit-rq? (ack-orig (rs-content resp)))
			     (begin
			       (gmsg "respond: quit detected.")
			       (server-quit client client path
					    'normal))
			     (gmsg "respond: no quit.")))))
		(begin
		  (gmsg #:priority 7 "respond: Client closed prematurely:" resp)
		  (close client)))))

	(define (receive)
	  (let ((result (list->record* (gread client))))
	    (gmsg #:priority 7 "receive: received:" result)
	    result))

	(gmsg #:priority 7 "loop: Got new client connection:" connection)
	(respond (receive)))
      (gmsg #:priority 5 "loop: Requests Processed:" iter)
      (loop (1+ iter)))

    (loop 1)))

(define (server-dispatcher request)
  "Interprets client requests, and passes additional information for
handling to request handler.

As scheme readers/writers currently don't handle records, objects(?)
or symbols, these need to be decomposed into lists before transmission
and recomposed upon receipt."

  (define (auth-provider rq)
    (let ((id (auth-id rq)))
      (if (id? id)
	  (let ((profile (select-gprofile id)))
	    (if profile
		(auth-rs profile)
		(neg-rs rq)))
	  (neg-rs rq))))

  (define (challenge-provider rq)
    (if (profile? (chall-rq-profile rq))
	(let* ((result (generate-challenge
			(check-profile (chall-rq-profile rq))))
	       (profile (car result))
	       (challenge (cdr result)))
	  (chall-rs profile challenge))
	(neg-rs rq)))

  (define (eval-provider rq)
    (let* ((result (generate-evaluation
		    (eval-rq-answer rq)
		    (check-profile (eval-rq-profile rq))))
	   (profile (car result))
	   (eval-result (cdr result)))
      (eval-rs profile eval-result)))

  (cond ((eof-object? request)
	 #f)
	((request? request)
	 (let ((rq (rq-content request)))
	   (cond ((alive-rq? rq)
		  (ack-rs rq))
		 ((profs-rq? rq)
		  (profs-rs (list-gprofiles)))
		 ((auth-rq? rq)
		  (auth-provider rq))
		 ((chall-rq? rq)
		  (challenge-provider rq))
		 ((eval-rq? rq)
		  (eval-provider rq))
		 ((quit-rq? rq)
		  (ack-rs rq))
		 (else (unk-rs rq)))))
	(else (unk-rs request))))

(define (server-quit s client path state)
  "Shuts down communication and stops the server. Tries to close
sockets gracefully even upon crash."
  (cond ((or (eq? state 'server)
	     (eq? state 'prepare-port))
	 (begin
	   (simple-format #t
			  "Exiting. Error: ~S"
			  state)
	   (newline)
	   (exit 1)))
	((eq? state 'normal)
	 (cond (s
		(begin
		  (if client
		      (close client))
		  (if s
		      (close s))
		  (if path
		      (delete-file path))
		  (simple-format #t
				 "Thank you for playing.")
		  (newline)
		  (exit #f #f #f 'normal)))
	       (else
		(exit 0))))
	(else
	 (begin
	   (simple-format #t 
			  "Received kill signal. Now quitting.
Arguments were: s ~S; client ~S; path ~S; state ~S."
			  s client path state)
	   (newline)
	   ;; (if client
	   ;;     (close client))
	   ;; (if s
	   ;;     (close s))
	   ;; (if path
	   ;;     (delete-file path))
	   (exit 1)))))
