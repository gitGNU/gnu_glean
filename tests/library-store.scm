;; library-store.scm --- library-store unit tests    -*- coding: utf-8 -*-
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
;; Library-store unit tests.
;;
;;; Code:

(define-module (tests library-store)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (glean common base-requests)
  #:use-module (glean common library-requests)
  #:use-module (glean library library-store)
  #:use-module (glean library server)
  #:use-module (ice-9 match)
  #:use-module (tests quickcheck-defs)
  #:use-module (quickcheck quickcheck))

(define (server-dispatcher rq)
  ((@@  (glean library server) server-dispatcher) rq))

(test-begin "library-store")

(test-assert "make hashmap"
  (quickcheck (lambda (_)
                (hashtree? (make-hashtree _)))
              50 $mk-rootset))
(test-assert "make deep hashmap"
  (quickcheck (lambda (_)
                (hashtree? (make-hashtree _)))
              50 $mk-set))

(test-end "library-store")

;;; discipline ends here
