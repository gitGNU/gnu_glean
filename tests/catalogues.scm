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
(define tmpcat (tmpnam))
(define tmpcurr (tmpnam))
(define tmpstore (tmpnam))

(define (tdisc)
  (define tdisc1 (string-join '("tests" "test-data-root" "test-discipline")
                              file-name-separator-string))
  (define tdisc2 (string-join '("test-data-root" "test-discipline")
                              file-name-separator-string))
  (if (file-exists? tdisc1) tdisc1 tdisc2))


;;;; Tests

(test-begin "catalogues")

;;;; Tests for: make-bare-catalogue

(test-assert "make-bare-catalogue"
  (match (make-bare-catalogue "test-cat" "tmpcat")
    (($ catalogue "test-cat" (? vlist-null?) "tmpcat") #t)
    (_ #f)))

;;;; Tests for: catalogue-add-discipline

(test-assert "catalogue-add-discipline"
  (let ((proc (@@ (glean librarian catalogues) catalogue-add-discipline)))
    (match (proc (make-bare-catalogue "test-catalogue-1" "tmpcat")
                 '("test-disc" . "/tmp/test/path"))
      (($ catalogue "test-catalogue-1" disciplines "tmpcat")
       (equal? (vlist->list disciplines)
               '(("test-disc" . "/tmp/test/path")))))))

;;;; Tests for: augment-catalogue

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

;;;; Tests for: impair-catalogue

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

;;;; Tests for: log-add-catalogue

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

;;;; Tmp directory Stateful tests

;;;; Tests for: tmp-cleaner

(test-assert "tmp-cleaner"
  (let ((file1 (tmpnam))
        (file2 (tmpnam)))
    (for-each mkdir `(,file1 ,file2))
    (= (((@@ (glean librarian catalogues) tmp-cleaner) file1 file2) tmpcat)
       2)))

;;;; Tests for: manalyzer

(test-assert "manalyzer"
  (match ((@@ (glean librarian catalogues) manalyzer) tmpcat tmpcurr (tdisc))
    ((? string?) #t)
    (_ #f)))

;;;; Tests for: mcatalogue-install

(test-assert "mcatalogue-install"
  (begin
    (mkdir tmpstore)                    ; destination dir must exist.
    (mkdir tmpcat)                      ; catalogues dir must exist.
    (match ((@@ (glean librarian catalogues) mcatalogue-install) tmpcat tmpcurr
            tmpstore (tdisc))
      (($ catalogue "catalogue-1" (? vhash?) (? string?)) #t)
      (_ #f))))

(test-end "catalogues")

(for-each (lambda (filename)
            (when (file-exists? filename)
              (system* "rm" "-r" filename)))
          ;; Order of deletion is important, as a different order would break
          ;; symlinks and broken symlinks fail `file-exists?'
          (list tmpcurr tmpcat tmpstore))

;;;; Missing Tests
;;;
;;; No tests have been written for procedures that relay in IO:
;;; log-add-discipline, catalogue-lister, catalogue-detailer,
;;; discipline-installer, current-catalogue-setter,
;;; next-catalogue-counter-maker, current-catalogue-namer.

;;; catalogues ends here
