;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;;; Lounge Filesystem — Lounge-store → Filesystem Interface

;; Copyright © 2012, 2014 Alex Sassmannshausen
;;
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

;;;; Commentary:
;;;
;;; Provide 'compile-lounge' and 'write-pdiff', 2 procedures that
;;; should handle all i/o to the underlying storage media.
;;;
;;;; Code:

(define-module (guilecraft lounge-filesystem)
  #:use-module (guilecraft config)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft utils)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:export (compile-lounge
            write-diff))

(define (make-brick diff)
  "A general brick. This could probably be killed. But does it add
clarity?"
  (lambda (lng-dir time)
    (match diff
      (('diff hash field value)
       (let ((place (string-append lng-dir "/" hash)))
         (mkdir-p place)
         (mkdir-p (string-append place "/actives"))
         (mkdir-p (string-append place "/counters"))
         (brick-write place diff time field))))))

(define (brick-write profile-dir diff time field)
  "A procedure to ensure CONTENTS that are valid get written to
PLACE."
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
  "Ensure the folder identified by the string HASH exists in LNG-DIR,
then ask BRICK to write itself to that folder, against TIME."
  (brick lng-dir time))

(define (write-diff diff lng-dir time)
  "Use HASH, PROFILE, FIELD, VALUE and OLDHASH to determine what needs
to be written to disk in LNG-DIR. The return value is unspecified."
  (fire-kiln (make-brick diff) lng-dir time))

;;; General Method:
;;; - Traverse lounge dir.
;;; - For each hash encountered, traverse and load data.
;;; - Once all data is loaded and the relevance of the profile
;;;   established (e.g. no burn-brick),
;;; - feed result of this data profile producing procedures
;;;   (make-profile, make-scorecard, update-scorecard etc.).
;;; - Move to next hash.
(define (compile-lounge lng-dir)
  ;; Following to consider: 
  ;; If a hash contains a burn-brick, ignore the hash.
  ;;
  ;; A hash could contain a link to a previous hash. If this is so we
  ;; need to build the profile on the basis of current content and
  ;; previous content. Either: checkpoint previous hash results in
  ;; current hash (above) or traverse down, then build profile from
  ;; there.  The lowest profile may already have been processed, as it
  ;; will contain nothing to indicate that it is part of a compound
  ;; profile at present (could be solved by adding a link to the new
  ;; hash to the old hash).  In fact, at present, each lower level
  ;; hash is already processed by the time we get to the later hashes,
  ;; because, hopefully ftw walks the directory according to simple
  ;; sort (i.e. older files are loaded first, because their filename
  ;; is smaller).  This means that continuing to work on a profile
  ;; should be as simple as locating that profile in the already
  ;; loaded data, and changing its hash (?).
  ;;
  ;; When processing counters we need to consider #t/#f (to be passed
  ;; to update-scorecard) and 0 values. The latter mean that the
  ;; counter file we are considering is newly added to the scorecard
  ;; -> create new entry in scorecard.  Using update-scorecard should
  ;; take care of percolating the scores, as we add them, to non-root
  ;; and crownsets.
  ;;
  ;; Loading actives files and counters files should be able to be
  ;; done in parallel.
  ;; 
  ;; Perform ftw etc.
  vlist-null)

