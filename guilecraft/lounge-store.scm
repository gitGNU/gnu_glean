;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;;; Lounge Store — Lounge → Filesystem Interface

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
;;; Provide functionality to manage lounge state and to interface with
;;; the filesystem.
;;;
;;;; Code:

(define-module (guilecraft lounge-store)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft monads)
  #:use-module (guilecraft utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-9)
  #:export (
            extract

            pdiff
            lounge-monad

            login
            authenticate
            fetch-lounge
            update-lounge
            ))

(define-record-type <pdiff>
  (pdiff hash profile field value)
  pdiff?
  (hash      pdiff-hash)
  (profile   pdiff-profile)
  (field     pdiff-field)
  (value     pdiff-value))

;;; for monads:
;; synonym for stateful, to force list around value(s)
(define (statef value state)
  (stateful (list value) state))

;; synonym for result, to take into account nothing possibility.
(define (extract st8teful)
  "Retrieve the result from ST8TEFUL, taking into account nothings. If
result contains only one item, extract that from the result list."
  (let ((prelim (result st8teful)))
    (if (null? (cdr prelim))
        (if (nothing? (car prelim))
            (reify nothing)
            (car prelim))
        prelim)))

;;;; Lounge Monad
;; A specialised monad providing logging, exception (FIXME: and file
;; locking) management.
(define (lounge-return . args)
  "Return a store mvalue seeded with ARGS."
  (lambda (lng-dir)
    "Return a stateful created using args."
    (if (null? (cdr args))
        (statef (car args) lng-dir)
        (stateful args lng-dir))))

(define (lounge-bind mvalue mproc)
  "Return a lounge mvalue, in turn capable of returning the result of
applying MVALUE to MPROC."
  (lambda (lng-dir)
    (let* ((new-stateful (mvalue lng-dir)) ; generate next stateful
           (reslt        (result new-stateful)))
      (cond ((nothing? (car reslt))
             new-stateful)
            ;; As lounge should never be modified by mvalue (that
            ;; would mean that lounge logic would be carrying out
            ;; state updates — these should be done at monad level),
            ;; the LNG passed to lounge-bind will be identical to LNG
            ;; coming out of new-stateful.
            (else ((apply mproc reslt) lng-dir))))))

(define-monad lounge-monad
  (bind   lounge-bind)
  (return lounge-return))

;;;;; Monadic Procedures
(define (login hash)
;; Should return mval
  (lambda (lng-dir)
    (let* ((tk  (lounge lng-dir hash))
           (ret (if tk tk (nothing 'unknown-user '()))))
      (statef ret lng-dir))))
(define (authenticate token)
  (lambda (lng-dir)
    (let* ((tk  (lounge lng-dir token))
           (ret (if tk tk (nothing 'unknown-user '()))))
      (statef ret lng-dir))))
(define (fetch-lounge)
  (lambda (lng-dir)
    (statef (lounge lng-dir) lng-dir)))
(define (update-lounge pdiff)
  (lambda (lng-dir)
    (statef (lounge lng-dir pdiff) lng-dir)))

;;;;; I/O Lounge Store Operations
;; A lounge is a database of all known sets indexed by their
;; fullhashes. A secondary index is stored in reference: minhash to
;; fullhash. The latter can be used to ensure continuity even after
;; set upgrades.

(define lounge
  (let ((profiles vlist-null)
        (tokens   vlist-null))
    (lambda*
     (lng-dir #:optional operation)
     (if (vlist-null? profiles)
         (set! profiles (compile-lounge lng-dir)))
     (cond ((not operation) profiles)
           ((eqv? 'tks operation) tokens)
           ((pdiff? operation)
            ;; use futures to write to disk as well as set!
            (write-pdiff lng-dir pdiff)
            (set! profiles (store-profile (pdiff-hash     operation)
                                          (pdiff-profile  operation)
                                          profiles)))
           ((token?        operation)
            (let ((tk-pair (renew-tk operation
                                     tokens
                                     (current-time))))
              (if tk-pair
                  (begin
                    (set! tokens (car tk-pair))
                    (cdr tk-pair))
                  #f)))
           (else
            (let ((tk-pair (fresh-tk operation
                                     tokens
                                     profiles
                                     (current-time))))
              (if tk-pair
                  (begin
                    (set! tokens (car tk-pair))
                    (cdr tk-pair))
                  #f)))))))

(define (compile-lounge lng-dir)
  ;; Perform ftw etc.
  vlist-null)

(define (write-pdiff lng-dir pdiff)
  "Write PDIFF to LNG-DIR according to contents of PDIFF. The return
value is unspecified."
  'undefined)

;;;;; Safe I/O Helpers
(define (store-profile hash profile profiles)
  (vhash-delete hash profiles)
  (vhash-cons hash profile profiles))

(define (fresh-tk hash tokens profiles time)
  (if (vhash-assoc hash profiles)
      (let ((tk  (make-token hash time))
            (tks (clear-tokens hash tokens)))
        (cons (vhash-cons tk hash tks) tk))
      #f))
(define (renew-tk token tokens time)
  (let ((auth-pair (vhash-assoc token tokens)))
    (if auth-pair
        (let ((tk (make-token (cdr auth-pair) time)))
          (cons (vhash-cons tk
                            (cdr auth-pair)
                            (vhash-delete token tokens))
                tk))
        #f)))

(define (token? obj) (number? obj))

(define (clear-tokens hash tokens)
  (vhash-fold (lambda (k v result)
                (if (string=? v hash)
                    result
                    (vhash-cons k v result)))
              vlist-null
              tokens))

(define (make-token hash time)
  "Return a transactional token generated from HASH and TIME."
(random (* 2 time)
        (seed->random-state
         (string-append hash
                        (number->string time)))))
