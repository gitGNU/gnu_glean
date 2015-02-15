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
;; Source-file: glean/library/library-store.scm
;;
;;; Code:

(define-module (tests library-store)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (glean common base-requests)
  #:use-module (glean common hash)
  #:use-module (glean common library-requests)
  #:use-module (glean library lexp)
  #:use-module (glean library library-store)
  #:use-module (glean library server)
  #:use-module (glean library sets)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (tests quickcheck-defs)
  #:use-module (quickcheck quickcheck))

(define server-dispatcher (@@  (glean library server) server-dispatcher))

(define index-set (@@ (glean library library-store) index-set))
(define library-cons (@@ (glean library library-store) library-cons))
(define empty-library (@@ (glean library library-store) empty-library))
(define <library> (@@ (glean library library-store) <library>))
(define hashtree? (@@ (glean library library-store) hashtree?))

(test-begin "library-store")

;;;; Tests for: index-set

(test-assert "index-set"
  (let loop ((rslt (index-set ($mk-discipline 3 3))))
    (match rslt
      ((((? hash?) (? set?) (? lexp?)) . rest)
       (if (null? rest) #t (loop rest)))
      (_ #f))))

;;;; Tests for: library-cons

(test-assert "library-cons"
  (let ((disc ($mk-discipline 3 3)))
    (match (library-cons disc (empty-library))
      (($ <library> catalogue reference)
       (match (vlist->list catalogue)
         ((((? hash?) ('set . (? set?)) ('lexp . (? lexp?))) ...)
          (match (vlist->list reference)
            ((((? lexp?) . (? set?)) ...)
             #t)
            (_  #f)))
         (_ #f)))
      (_ #f))))

;;;; Tests for: hashtree?

(test-assert "hashtree-true"
  (hashtree? '(("hash" . (test)))))

(test-assert "hashtree-deeper-true"
  (hashtree? '(("hash" . (test)) ((("hash" . (test)))))))

(test-assert "hashtree-false"
  (not (hashtree? '(blah blah))))

;;;; Tests for: make-hashtree

(test-assert "make hashmap"
  (let ((disc ($mk-rootset 10)))
    (hashtree? (make-hashtree disc (lexp-make (set-id disc))))))

(test-assert "make deep hashmap"
  (let ((disc ($mk-discipline 3 3)))
    (hashtree? (make-hashtree disc (lexp-make (set-id disc))))))

(test-end "library-store")

;;; discipline ends here
