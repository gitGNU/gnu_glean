;; base-server.scm --- basic server structure   -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen  <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
;;
;; This file is part of Glean.
;;
;; Glean is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; Glean is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with glean; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; Module to provide a daemon mode, whereby a glean server (e.g. lounge,
;; library) listens on a socket for requests.
;;
;;; Code:

(define-module (glean common base-server)
  #:use-module (glean common base-requests)
  #:use-module (glean common comtools)
  #:use-module (glean common utils)
  #:use-module (ice-9 match)
  #:use-module (rnrs records inspection)
  #:use-module (srfi srfi-26)
  #:export (the-server))


;;;; Variables & The-server
;;;
;;; Typically this library is used by glean parts implementing a daemonized
;;; server.  Examples are the lounge and the library servers.
;;; Server implementations should simply call (the-server $socket-file
;;; $server-dispatcher), where $server-dispatcher is the procedure actually
;;; implemented by the server itself.

(define (the-server socket-file server-dispatcher)
  "Initiates a server loop, with the server listening at SOCKET-FILE and its
actions defined by SERVER-DISPATCHER.

SERVER-DISPATCHER should be a procedure of one argument. That argument will
normally be a request but has not been parsed yet."
  ;; Check for existing socket files
  (when (file-exists? socket-file)
    (warning (_ "Encountered an existing socket.
Another instance of this Glean server might be running!

Continue? (y/n)"))
    (match (read)
      ((or 'y 'Y 'yes 'Yes 'YES)
       (delete-file socket-file))
      (_ (exit 1))))
  ;; Create socket
  (let ((socket    (socket PF_UNIX SOCK_STREAM 0))
        (sock-addr (make-socket-address AF_UNIX socket-file)))
    (setsockopt socket SOL_SOCKET SO_REUSEADDR 1)
    (bind socket sock-addr)
    (listen socket 5)
    ;; Catch kill signals
    (sigaction SIGTERM (lambda args (clean-exit socket socket-file args)))
    (sigaction SIGINT  (lambda args (clean-exit socket socket-file args)))
    ;; Start Server
    (gmsg #:priority 5
          "server-loop: listening for clients as pid:" (getpid))
    (server-loop socket server-dispatcher)
    ;; Close Server
    (clean-exit socket socket-file)))


;;;; Support Procedures

(define (clean-exit socket socket-file . args)
  "Remove sockets, if they exist, and exit."
  (close socket)
  (if (file-exists? socket-file)
      (begin (format #t "Deleting existing socket...")
             (delete-file socket-file)
             (format #t "[DONE]\n")))
  (exit))

(define (server-loop socket server-dispatcher)
  "Process requests arriving at SOCKET until a quit request is received.
Requests are processed by passing them to SERVER-DISPATCHER."
  (define (loop iter)
    (gmsg #:priority 5 "loop: now processing request number " iter)

    (if (match (accept socket)
          ((client . info)
           (match (list->record* (gread client))
             ((? eof-object?)
              (gwrite (record->list* #f) client)
              (close client))
             ((? request? rq)
              (format #t "Request: ~a\n" (rq-content rq))
              (match (server-dispatcher rq)
                ((? record? resp)
                 (format #t "Response: ~a\n" resp)
                 (gwrite (record->list* (response resp)) client)
                 (close client)
                 ;; Return #f if quitq
                 (not (and (acks? resp) (quitq? (ack-orig resp)))))
                (_ (error "SERVER-LOOP -- Unexpected dispatch response" _))))
             (unknown (gwrite (record->list* (unks unknown)) client)
                      (close client)))))
        (begin (gmsg #:priority 5 "loop: processed request number " iter)
               (loop (1+ iter)))
        (begin (gmsg #:priority 1 "Quit request: quitting.\n")
               #f)))

  (loop 1))

;;; base-server.scm ends here
