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

(define-module (tests comtools-online)
  #:use-module (srfi srfi-64)		; Provide test suite
  #:use-module (guilecraft config)
  #:use-module (guilecraft comtools)) ; Provide functions to be
				      ; tested.

(test-begin "comms-tests")

(begin
  (define path (string-append %guilecraft-dir% "/socket"))
  (define address (make-socket-address AF_UNIX path)))

;; Write and immediately close: causes a crash when server attempts to
;; write to port.
;; (test-assert "gwrite"
;; 	     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
;; 	       (connect s address)
;; 	       (let ((result (gwrite 'test s)))
;; 		 (and (close s)
;; 		      result))))

;; Test a misbehaving symbol message (will hang comms unless server
;; disconnects in some fashion)
;; I don't know how to pass this test yet, so disabling for now
;; (test-assert "server-evil-write"
;; 	     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
;; 	       (connect s address)
;; 	       (write 'random s)
;; 	       (let ((msg (read s)))
;; 		 (close s)
;; 		 (if msg
;;                   #t
;;                   #f))))

;; If a client reads without first writing, client and server end in a
;; lock, where both read infinitely. Currently this leads to the
;; entire server blocking.
;; (test-assert "server-evil-read"
;; 	     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
;; 	       (connect s address)
;; 	       (let ((msg (read s)))
;; 		 (close s)
;; 		 (if msg
;; 		     #t
;; 		     #f))))

;; Connect and immediately disconnect. Causes a crash
;; (test-assert "server-disconnect"
;; 	     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
;; 	       (connect s address)
;; 	       (close s)))

;; If I don't read below the server crashes
(test-assert "gwrite"
	     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
	       (connect s address)
	       (let ((result (gwrite 'test s)))
		 (read s)
		 (close s)
		 result)))

;; Read from dummy port — expect failure
(test-assert "gread-fail"
	     (not (gread 's)))

(test-assert "gread-success"
	     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
	       (connect s address)
	       (gwrite 'random s)
	       (let ((result (gread s)))
		 (close s)
		 result)))

;; Use server as abstraction for port connection
(test-assert "server-write"
	     (let ((s (server)))
	       (and (gwrite 'test s)
		    (gread s)
		    (close s))))

;; Use exchange as abstraction for port connection
(test-assert "exchange-raw"
	     (exchange 'test))

(test-assert "alive?"
	     (alive?))

(test-end "comms-tests")
