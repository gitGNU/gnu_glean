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
            write-pdiff))

(define (burning-brick)
  "Prepare a brick that should as its action delete the place it will
be provided with. For now it creates a 'fire-brick'. This will cause
problems."
  (brick (lambda (place time)
           (brick-write (string-append place "/" time "-fire")
                        (list 'fire place time)))))
(define (meta-brick name lounge library oldhash)
  "Prepare a brick that will as its action write updated account
meta information to thp place it will be provided with."
  (define (false-or-values . values)
    (map (lambda (value) (if value value 'false)) values))
  (brick (lambda (place time)
           (brick-write (string-append place "/" time "-meta")
                        (false-or-values 'meta name lounge library
                                         time))
           (if oldhash
               (brick-write (string-append place "/" oldhash "-link")
                            (list oldhash))))))
(define (active-brick active-modules)
  "Prepare a brick that will, when invoked, add information about the
active modules to the actives subdir of the place it is provided
with."
  (brick (lambda (place time)
           (brick-write (string-append place "/actives/"
                                       time "-active")
                        (list 'active-modules active-modules time)))))
(define (counter-brick blobhash assessment-result)
  "Prepare a brick that will, when invoked, add information on
BLOBHASH and ASSESSMENT-RESULT to the counters subdir of the place it
will be provided with."
  (brick (lambda (place time)
           (brick-write (string-append place "/counters/"
                                       time "-counter")
                        (list 'counter blobhash assessment-result
                              time)))))
(define (brick mortar)
  "A general brick. This could probably be killed. But does it add
clarity?"
  (lambda (place time)
    (mortar place (number->string time))))

(define (brick-write place contents)
  "A procedure to ensure CONTENTS that are valid get written to
PLACE."
  (define (make-counter counter)
    (let ((suffix (string-append "-" (number->string counter))))
      (if (access? (string-append place suffix) R_OK)
          (make-counter (1+ counter))
          suffix)))
  (if (and (string? place)
           (list    contents))
      (let ((counter (make-counter 0)))
        (with-output-to-file (string-append place counter)
          (lambda ()
            (write contents)
            (newline))))
      (error "Srsly? Check the brick code.")))

(define (kiln brick hash lng-dir time)
  "Ensure the folder identified by the string HASH exists in LNG-DIR,
then ask BRICK to write itself to that folder, against TIME."
  (let ((place (string-append lng-dir "/" hash)))
    (mkdir-p place)
    (mkdir-p (string-append place "/actives"))
    (mkdir-p (string-append place "/counters"))
    (brick place time)))

(define (write-pdiff hash profile field value oldhash lng-dir time)
  "Use HASH, PROFILE, FIELD, VALUE and OLDHASH to determine what needs
to be written to disk in LNG-DIR. The return value is unspecified."
  (cond ((eqv? field 'rescore)        ; Evaluation result
         (match value
           ((blobhash assessment-result)
            (kiln (counter-brick blobhash assessment-result)
                  hash lng-dir time))))
        ((eqv? field 'scorecard)
         (for-each (lambda (blobhash)
                     (kiln (counter-brick blobhash 0)
                           hash lng-dir time))
                   value))
        ((eqv? field 'register)       ; Create profile
         (kiln (meta-brick (profile-name profile)
                           (profile-prof-server profile)
                           (profile-mod-server profile)
                           #f)
               hash lng-dir time))
        ((eqv? field 'active-modules)       ; Active Modules
         (kiln (active-brick value) hash lng-dir time))
        ((eqv? field 'name)       ; New name
         (kiln (meta-brick value #f #f oldhash) hash lng-dir time))
        ((eqv? field 'password)    ; New password
         (kiln (meta-brick #f #f #f oldhash) hash lng-dir time))
        ((eqv? field 'prof-server) ; Lounge
         (kiln (meta-brick #f value #f #f) hash lng-dir time))
        ((eqv? field 'mod-server)  ; Library update
         (kiln (meta-brick #f #f value #f) hash lng-dir time))
        ((eqv? field 'delete)      ; Delete profile
         (kiln (burning-brick) hash lng-dir time))
        (else
         (error 'No-such-brick-type))))

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

