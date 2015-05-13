;; hash.scm --- hash unit tests    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
;; Hash unit tests.
;;
;;; Code:

(define-module (tests hash)
  #:use-module (glean common hash)
  #:use-module (glean common base32)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports))

;; Test the (glean hash) module.

(define %empty-sha256
  ;; SHA256 hash of the empty string.
  (base32-string->bytevector
   "4oymiquy7qobjgx36tejs35zeqt24qpemsnzgtfeswmrw6csxbkq"))

(define %hello-sha256
  ;; SHA256 hash of "hello world"
  (base32-string->bytevector
   "xfgspomtju7arjjokll5u7nl7lcij37dpjjyb3uqrd32zyxpzxuq"))

(define (supports-unbuffered-cbip?)
  "Return #t if unbuffered custom binary input ports (CBIPs) are supported.
In Guile <= 2.0.9, CBIPs were always fully buffered, so the
'open-sha256-input-port' does not work there."
  (false-if-exception
   (setvbuf (make-custom-binary-input-port "foo" pk #f #f #f) _IONBF)))


(test-begin "hash")

(test-equal "sha256, empty"
  %empty-sha256
  (sha256 #vu8()))

(test-equal "sha256, hello"
  %hello-sha256
  (sha256 (string->utf8 "hello world")))

(test-equal "open-sha256-port, empty"
  %empty-sha256
  (let-values (((port get)
                (open-sha256-port)))
    (close-port port)
    (get)))

(test-equal "open-sha256-port, hello"
  %hello-sha256
  (let-values (((port get)
                (open-sha256-port)))
    (put-bytevector port (string->utf8 "hello world"))
    (get)))

(test-skip (if (supports-unbuffered-cbip?) 0 4))

(test-equal "open-sha256-input-port, empty"
  `("" ,%empty-sha256)
  (let-values (((port get)
                (open-sha256-input-port (open-string-input-port ""))))
    (let ((str (get-string-all port)))
      (list str (get)))))

(test-equal "open-sha256-input-port, hello"
  `("hello world" ,%hello-sha256)
  (let-values (((port get)
                (open-sha256-input-port
                 (open-bytevector-input-port
                  (string->utf8 "hello world")))))
    (let ((str (get-string-all port)))
      (list str (get)))))

;;; FIXME: use base32 instead of base16
;; (test-equal "open-sha256-input-port, hello, one two"
;;   (list (string->utf8 "hel") (string->utf8 "lo")
;;         (base16-string->bytevector                ; echo -n hello | sha256sum
;;          "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
;;         " world")
;;   (let-values (((port get)
;;                 (open-sha256-input-port
;;                  (open-bytevector-input-port (string->utf8 "hello world")))))
;;     (let* ((one   (get-bytevector-n port 3))
;;            (two   (get-bytevector-n port 2))
;;            (hash  (get))
;;            (three (get-string-all port)))
;;       (list one two hash three))))

;; (test-equal "open-sha256-input-port, hello, read from wrapped port"
;;   (list (string->utf8 "hello")
;;         (base16-string->bytevector                ; echo -n hello | sha256sum
;;          "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
;;         " world")
;;   (let*-values (((wrapped)
;;                  (open-bytevector-input-port (string->utf8 "hello world")))
;;                 ((port get)
;;                  (open-sha256-input-port wrapped)))
;;     (let* ((hello (get-bytevector-n port 5))
;;            (hash  (get))

;;            ;; Now read from WRAPPED to make sure its current position is
;;            ;; correct.
;;            (world (get-string-all wrapped)))
;;       (list hello hash world))))

;;;; Hash predicates

(test-assert "hash=?"
  (and (hash=? "test" "test")
       (hash=? '("test" . "test"))
       (not (hash=? "" "test"))
       (not (hash=? "" ""))
       (not (hash=? #f #f))))

(test-assert "hash&!=?"
  (and (hash&!=? "test" "blah")
       (not (hash&!=? "" "test"))
       (not (hash&!=? "test" "test"))
       (not (hash&!=? #f "test"))))

(test-end)


;;(exit (= (test-runner-fail-count (test-runner-current)) 0))


;;; discipline ends here
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
