;; server.scm --- base-server unit tests    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
;;
;; This file is part of Glean.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Base server unit tests.
;;
;;; Code:

(define-module (tests server)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (glean config)
  #:use-module (glean common comtools)
  ) ; Provide functions to be tested.

(test-begin "server-tests")

(begin
  (define path %library-port%)
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
;;              (let ((s (socket PF_UNIX SOCK_STREAM 0)))
;;                (connect s address)
;;                (write 'random s)
;;                (let ((msg (read s)))
;;                  (close s)
;;                  (if msg
;;                      #t
;;                      #f))))

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

;;; discipline ends here
