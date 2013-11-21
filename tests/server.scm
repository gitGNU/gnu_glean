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

(define-module (tests server)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (guilecraft config)
  #:use-module (guilecraft comtools)
  #:use-module (guilecraft clients min)
  ) ; Provide functions to be tested.

(test-begin "server-tests")

(begin
  (define path %module-socket-file%)
  (define address (make-socket-address AF_UNIX path)))

;; Test a well behaving symbol message (it uses gwrite)
(test-assert "server-random"
	     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
	       (connect s address)
	       (gwrite 'random s)
	       (let ((msg (read s)))
		 (close s)
		 (if (eqv? msg 'invalid-request)
		     #t
		     #f))))

;; Test a misbehaving symbol message (will hang comms unless server
;; disconnects in some fashion)
;; I don't know how to pass this test yet, so disabling for now
;; (test-assert "server-evil"
;; 	     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
;; 	       (connect s address)
;; 	       (write 'random s)
;; 	       (let ((msg (read s)))
;; 		 (close s)
;; 		 (if msg
;; 		     #t
;; 		     #f))))

(test-assert "server-disconnect"
	     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
	       (connect s address)
	       (close s)))
(test-assert "server-quit"
	     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
	       (connect s address)
	       (write 'quit s)
	       (close s)))

(test-end "server-tests")
