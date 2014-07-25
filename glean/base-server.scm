;;; glean --- fast learning tool.         -*- coding: utf-8 -*-

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
;; Module to provide daemon mode, whereby glean acts as a server,
;; listening on a socket for requests.
;;
;;; Code:

(define-module (glean base-server)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-26)
  #:use-module (glean data-types base-requests)
  #:use-module (glean comtools)
  #:use-module (glean utils)
  #:use-module (rnrs)
  #:export (the-server))

(define %gettext-domain
  "glean")

(define _ (cut gettext <> %gettext-domain))
(define N_ (cut ngettext <> <> <> %gettext-domain))

(define (clean-and-exit socket-file . args)
  "Remove sockets, if they exist, and exit."
  (define (del-sock-exit sock-file)
    (display "Found socket. Deleting...")
    (newline)
    (delete-file sock-file)
    (display "Deleted. Exiting.")
    (newline)
    (exit))
  (define (base-exit)
    (display "No sockets open. Exiting.")
    (newline)
    (exit))

  args ;; ignore args
  (display "Caught term signal. Checking for sockets...")
  (newline)
  (cond ((file-exists? socket-file)
         (del-sock-exit socket-file))
        (else (base-exit))))

(define (the-server socket-file server-dispatcher)
  "Starts server: sets socket path, calls checks and then server
loop."
  (define (thunk)
    (if (prepare-port socket-file)
	(server-loop socket-file server-dispatcher)))
  (define (h key . args)
    (display args)
    (newline)
    (let ((socket (car args))
	  (client (cadr args))
	  (path (caddr args))
	  (state (cadddr args)))
      (server-quit socket client path state)))
  
  ;; start the server
  ;;(catch #t thunk h)
  (sigaction SIGTERM (lambda args (clean-and-exit socket-file args)))
  (sigaction SIGINT (lambda args (clean-and-exit socket-file args)))
  (thunk)
  )

(define (prepare-port socket-file)
  "Checks whether SOCKET-FILE exists, and if so, offers to delete and
continue load."
  (if (not (file-exists? socket-file))
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
		(delete-file socket-file)
		#t)
	      (throw 'socket-error #f #f socket-file 'prepare-port))))))

(define (server-loop socket-file server-dispatcher)
  "Set up the socket on SOCKET-FILE, and use loop to accept and
forward client requests to SERVER-DISPATCHER."

  (define (start-server)
    (let ((s (socket PF_UNIX SOCK_STREAM 0))
	  (sock-addr (make-socket-address AF_UNIX socket-file)))

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
	  (clog "in respond")
	  (let ((resp (response (server-dispatcher record))))
	    (gmsg #:priority 7 "respond: resp:" resp)
	    (gmsg #:priority 7 "respond: resp:" (rs-content resp))
	    (rprinter resp)
	    (if resp
		(begin
		  (gwrite (record->list* resp) client)
		  (close client)
		  (gmsg #:priority 8 "respond: Client closed.")
		  (cond ((acks? (rs-content resp))
			 (gmsg #:priority 7 "respond: checking for quitq…")
			 (if (quitq? (ack-orig (rs-content resp)))
			     (begin
			       (gmsg "respond: quit detected.")
			       (server-quit client client socket-file
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

(define (server-quit s client socket-file state)
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
		  (if socket-file
		      (delete-file socket-file))
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
Arguments were: s ~S; client ~S; socket-file ~S; state ~S."
			  s client socket-file state)
	   (newline)
	   ;; (if client
	   ;;     (close client))
	   ;; (if s
	   ;;     (close s))
	   ;; (if socket-file
	   ;;     (delete-file socket-file))
	   (exit 1)))))
