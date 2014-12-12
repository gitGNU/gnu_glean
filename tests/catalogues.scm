;; catalogues.scm --- tests for catalogues    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 07 December 2014
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
;; Unit tests for catalogues.
;;
;; Source-file: glean/librarian/catalogues.scm
;;
;;; Code:

(define-module (tests catalogues)
  #:use-module (glean librarian catalogues)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-64)
  )


;;;; Utilities
(define catalogue (@@ (glean librarian catalogues) catalogue))
(define make-bare-catalogue
  (@@ (glean librarian catalogues) make-bare-catalogue))


;;;; Tests

(test-begin "catalogues")

(test-assert "make-bare-catalogue"
  (match (make-bare-catalogue "test-cat")
    (($ catalogue "test-cat" (? vlist-null?)))))

(test-assert "catalogue-add-discipline"
  (let ((proc (@@ (glean librarian catalogues) catalogue-add-discipline)))
    (match (proc (make-bare-catalogue "test-catalogue-1")
                 '("test-disc" . "/tmp/test/path"))
      (($ catalogue "test-catalogue-1" disciplines)
       (equal? (vlist->list disciplines)
               '(("test-disc" . "/tmp/test/path")))))))

(test-assert "augment-catalogue"
  (let ((proc (@@ (glean librarian catalogues) augment-catalogue)))
    (and (match (proc (list (make-bare-catalogue "catalogue-1"))
                      5 "/tmp/test/path")
           (($ catalogue "catalogue-5" disciplines)
            (equal? (vlist->list disciplines)
                    '(("path" . "/tmp/test/path")))))
         (match (proc '() 5 "/tmp/test/path")
           (($ catalogue "catalogue-5" disciplines)
            (equal? (vlist->list disciplines)
                    '(("path" . "/tmp/test/path"))))))))

(test-assert "impair-catalogue"
  (let ((proc (@@ (glean librarian catalogues) impair-catalogue))
        (aug  (@@ (glean librarian catalogues) augment-catalogue)))
    (and
     (match (proc (list (make-bare-catalogue "catalogue-1")) 5 "test-disc")
       (($ catalogue "catalogue-5" disciplines)
        (null? (vlist->list disciplines))))
     (match (proc (list (aug (list (make-bare-catalogue "catalogue-1"))
                             5 "/tmp/test/path"))
                  6 "path")
       (($ catalogue "catalogue-6" disciplines)
        (null? (vlist->list disciplines)))))))

(test-assert "log-add-catalogue"
  (let ((proc (@@ (glean librarian catalogues) log-add-catalogue))
        (bare-cat (@@ (glean librarian catalogues) make-bare-catalogue)))
    (and (match (proc "test-cat" '())
           ((($ catalogue "test-cat" vlist-null)) #t))
         (match (proc "test-cat-2" `(,(bare-cat "test-cat")))
           ((($ catalogue "test-cat-2" vlist-null)
             ($ catalogue "test-cat" vlist-null))
            #t)))))

(test-end)

;;;; Missing Tests
;;;
;;; No tests have been written for procedures that relay in IO:
;;; log-add-discipline, catalogue-lister, catalogue-detailer,
;;; discipline-installer, current-catalogue-setter,
;;; next-catalogue-counter-maker, current-catalogue-namer.

;;; catalogues ends here
