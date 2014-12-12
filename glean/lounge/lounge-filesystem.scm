;; lounge-filesystem.scm --- Lounge-store → Filesystem  -*- coding: utf-8 -*-
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
;; Provide `compile-lounge' and `write-pdiff', 2 procedures that
;; should handle all i/o to the underlying storage media.
;;
;; This is a first version of an interface to persistent storage.  My aim is
;; to implement persistent storage as a 'component', so that different core
;; and extension mechanisms can be used as drop in replacements.  At present
;; the minimum interface provided by those components is `compile-lounge',
;; which should return a complete lounge catalogue as represented in the
;; file-system, and `write-diff', a procedure that is given a lounge
;; transaction, which should ensure that that transaction is recorded in a
;; persistent manner, for use by the `compile-lounge' procedure.
;;
;; Future interface additions could include statistical and export/import
;; functionality.
;;
;; A good argument could be made that `compile-lounge' is to general: one
;; could imagine a sql based backend which would suffer from having to simply
;; dump the entire store into a vhash data-structure.  A more realistic
;; approach in future might be to require 'goal' oriented interface
;; requirements, which are then implemented at component level
;; (e.g. `fetch-profile-summary', `fetch-active-modules', etc.).
;;
;;; Code:

(define-module (glean lounge lounge-filesystem)
  #:use-module (glean common utils)
  #:use-module (glean lounge lounge-store)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (
            compile-lounge
            write-diff
            ))


;;;;; Overview
;;; The persistent storage implemented by this module is a plain folder/file
;;; system.  Individual transactions are recorded in the subdirectory of it's
;;; profile, one transaction per file.
;;;
;;; `compile-lounge' traverses the filesystem tree and reconstitutes the
;;; profiles that have been stored by 'replaying' each transaction.

;;;;; Porcelain

;;;; Write-diff
(define (write-diff diff lng-dir time)
  "Write the tagged list DIFF to the appropriate folder in LNG-DIR (a string),
with timestamp TIME. The return value is unspecified."
  (fire-kiln (make-brick diff) lng-dir time))

;;;; Compile Lounge: initial loading of previous session data.
(define (compile-lounge lng-dir)
  "Traverse the directory identified by the string LNG-DIR, interpreting each
top level folder as a profile, and its name as a hash.  Each file within each
of these folders is considered a transaction to be 'replayed'."
  (define (enter? . args) #t)
  (define (leaf path stat result)
    (cons path result))
  (define (down path stat result) result)
  (define (up path stat result) result)
  (define (skip path stat result) result)
  (define (error path stat errno result)
    (format #t "~a\n~a\n~a\n~a\n" path stat errno result))

  (call-with-values
      (lambda ()
        (file-system-fold enter? leaf down up skip error '() lng-dir))
    (lambda (result overview)
      (fold (lambda (filename result)
              (match (with-input-from-file filename read)
                ((diff stamp)
                 (cdr (store-profile diff result)))
                (_ (format #t "Failed to match: ~a\n" (basename filename))
                   #f)))
            vlist-null
            (sort result (lambda (filepath1 filepath2)
                           (string<? (basename filepath1)
                                     (basename filepath2))))))))


;;;; Helpers
;;; These are all related to write-diff.

(define (make-brick diff)
  "Return a procedure which, when given a location and a timestamp, will write
the tagged list DIFF to the file-system."
  (lambda (lng-dir time)
    (match diff
      (('diff hash field value)
       (let ((place (string-append lng-dir "/" hash)))
         (mkdir-p place)
         (mkdir-p (string-append place "/actives"))
         (mkdir-p (string-append place "/counters"))
         (brick-write place diff time field))))))

(define (brick-write profile-dir diff time field)
  "Write the tagged list DIFF to a file named by the string PROFILE-DIR, the
numeric timestamp TIME, the symbol FIELD and a counter generated on the fly,
to the file-system."
  (let ((place (string-append profile-dir "/" (number->string time)
                              "-" (symbol->string field))))
    (define (make-counter counter)
      (let ((suffix (string-append "-" (number->string counter))))
        (if (access? (string-append place suffix) R_OK)
            (make-counter (1+ counter))
            suffix)))

    (with-output-to-file (string-append place (make-counter 0))
      (lambda ()
        (write (list diff time))
        (newline)))))

(define (fire-kiln brick lng-dir time)
  "Apply BRICK to LNG-DIR and TIME.  The return value is unspecified, but the
side-effect should be the writing of a transaction contained in brick to the file-system."
  (brick lng-dir time))

;;; lounge-filesystem.scm ends here
