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
  #:use-module (guilecraft lounge-store)
  #:use-module (guilecraft monads)
  #:use-module (guilecraft utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
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


;;;; Compile Lounge: initial loading of previous session data.
;;; As we have stored the data as diffs enclosed in a list, loading the data
;;; should be simple: simply read the data and pass the diff and pass it to
;;; the normal processing procedure for a new diff: update-lounge.
;;;
;;; General Method:
;;; 
;;; - Traverse lounge dir.
;;; - For each hash-dir encountered, traverse it, open each file (ignoring
;;; folders for now), read list, extract diff and pass to update-lounge.
;;; - Move to next hash.
(define (compile-lounge lng-dir)
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

