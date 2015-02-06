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
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  )


;;;; Utilities
(define make-bare-catalogue
  (@@ (glean librarian catalogues) make-bare-catalogue))
(define augment-catalogue
  (@@ (glean librarian catalogues) augment-catalogue))
(define tdisc (string-join '("tests" "test-data-root" "test-discipline")
                           file-name-separator-string))
(define tmpcat (tmpnam))
(define tmpcurr (tmpnam))
;;; Naive attempt to ensure we run from correct directory, so that naive
;;; deletion at EOF works.
;; (when (not (file-exists? tdisc))
;;   (format #t "We did not run the tests from the expected directory. problem
;; that we hoped might not occur occurred...")
;;   (exit 1))


;;;; Tests

(test-begin "catalogues")

(test-assert "make-bare-catalogue"
  (match (make-bare-catalogue "test-cat" "tmpcat")
    (($ catalogue "test-cat" (? vlist-null?) "tmpcat") #t)
    (_ #f)))

;;;;; Catalogue Add Discipline
(test-assert "catalogue-add-discipline"
  (let ((proc (@@ (glean librarian catalogues) catalogue-add-discipline)))
    (match (proc (make-bare-catalogue "test-catalogue-1" "tmpcat")
                 '("test-disc" . "/tmp/test/path"))
      (($ catalogue "test-catalogue-1" disciplines "tmpcat")
       (equal? (vlist->list disciplines)
               '(("test-disc" . "/tmp/test/path")))))))

;;;;; Augment Catalogue
(test-assert "augment-catalogue"
  (match (augment-catalogue (make-bare-catalogue "catalogue-1" "tmpcat")
                            5 "id" "/tmp/test/hash-id-version")
    (($ catalogue "catalogue-5" disciplines "tmpcat")
     (equal? (vlist->list disciplines)
             '(("id" . "/tmp/test/hash-id-version"))))))

(test-assert "augment-catalogue-tmp"
  (match (augment-catalogue (make-bare-catalogue "catalogue-1" "tmpcat")
                            5 "id" "/tmp/test/id" "/tmp/cat")
    (($ catalogue "catalogue-5" disciplines "/tmp/cat")
     (equal? (vlist->list disciplines)
             '(("id" . "/tmp/test/id"))))))

(test-assert "augment-catalogue-type-failure"
  (catch 'glean-type-error
    (cut augment-catalogue '() 5 "id" "/tmp/test/hash-id-version")
    (lambda args #t)))

;;;;; Impair Catalogue
(test-assert "impair-catalogue"
  (let ((proc (@@ (glean librarian catalogues) impair-catalogue)))
    (and (match (proc (make-bare-catalogue "catalogue-1" "/tmp/cat")
                      5 "test-disc")
           (($ catalogue "catalogue-5" disciplines)
            (null? (vlist->list disciplines))))
         (match (proc (augment-catalogue (make-bare-catalogue "catalogue-1"
                                                              "/tmp/cat")
                                         5 "id" "/tmp/test/hash-id-version")
                      6 "id")
           (($ catalogue "catalogue-6" disciplines)
            (null? (vlist->list disciplines)))))))

;;;;; Log Add Catalogue
(test-assert "log-add-catalogue"
  (let ((proc (@@ (glean librarian catalogues) log-add-catalogue)))
    (and (match (proc "test-cat" '() "/tmp/cat")
           ((($ catalogue "test-cat" vlist-null)) #t))
         (match (proc "test-cat-2"
                      `(,(make-bare-catalogue "test-cat" "/tmp/cat"))
                      "/tmp/cat")
           ((($ catalogue "test-cat-2" vlist-null "/tmp/cat")
             ($ catalogue "test-cat" vlist-null "/tmp/cat"))
            #t)))))

(test-assert "tmp-catalogue"
  (match (mcatalogue-tmp tmpcat tmpcurr tdisc)
    (($ catalogue "catalogue-0" (? vhash?) (? string? tmp))
     ;; Once again, we're leaving behind random tmp files as long as we have
     ;; no tmp-catalogue cleaner procedure.
     #t)
    (_ #f)))

(test-end)

;;; Naive deletion of temporary files in attempt to restrain number of test
;;; files in tmp
;; (for-each (lambda (file)
;;             (when (and (file-exists? file)
;;                        (string=? (dirname file) "/tmp"))
;;               (system* "rm" "-r" file)))
;;           `(,tmpcurr ,tmpcat))

;;;; Missing Tests
;;;
;;; No tests have been written for procedures that relay in IO:
;;; log-add-discipline, catalogue-lister, catalogue-detailer,
;;; discipline-installer, current-catalogue-setter,
;;; next-catalogue-counter-maker, current-catalogue-namer.

;;; catalogues ends here
