;; profiles.scm --- profile data type & operations    -*- coding: utf-8 -*-
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
;; Profiles are the basic data-type for the lounge.  They contain a player's
;; meta-data and, in the active-modules and scorecard fields, a diary of their
;; progress and activated disciplines.
;;
;; This module currently uses rnrs record types, which I would like to migrate
;; to srfi-9.
;;
;;; Code:

(define-module (glean lounge profiles)
  #:use-module (glean common hash)
  #:use-module (glean common utils)
  #:use-module (glean lounge scorecards)
  #:use-module (rnrs records procedural)
  #:use-module (srfi srfi-1)
  #:export (
            make-profile
            make-bare-profile
            profile?
            profile-name
            profile-prof-server
            profile-mod-server
            profile-active-modules
            profile-scorecard

            parse-active-modules
            no-active-modules?
            profile-hash
            update-profile
            ))


;;;;; Record Types

(define profile-rtd
  (make-record-type-descriptor 'profile #f #f #f #f
                               '#((immutable name)
                                  (immutable prof-server)
                                  (immutable mod-server)
                                  (immutable active-modules)
                                  (immutable scorecard))))

(define profile-rcd
  (make-record-constructor-descriptor
   profile-rtd #f
   (lambda (new)
     (lambda (name prof-server mod-server active-modules scorecard)
       (cond ((not (string? name))
              (raise '(make-profile invalid-name)))
             ((not (string? prof-server))
              (raise '(make-profile invalid-prof-server)))
             ((not (string? mod-server))
              (raise '(make-profile invalid-mod-server)))
             ((not (list? active-modules))
              (raise '(make-profile invalid-active-modules)))
             ((not (scorecard? scorecard))
              (raise '(make-profile invalid-scorecard)))
             (else (new name prof-server mod-server active-modules
                        scorecard)))))))

(define bare-profile-rcd
  (make-record-constructor-descriptor
   profile-rtd #f
   (lambda (new)
     (lambda (name prof-server mod-server)
       (new name prof-server mod-server '() (make-empty-scorecard))))))

(define make-profile (record-constructor profile-rcd))
(define make-bare-profile (record-constructor bare-profile-rcd))
(define profile? (record-predicate profile-rtd))
(define profile-name (record-accessor profile-rtd 0))
(define profile-prof-server (record-accessor profile-rtd 1))
(define profile-mod-server (record-accessor profile-rtd 2))
(define profile-active-modules (record-accessor profile-rtd 3))
(define profile-scorecard (record-accessor profile-rtd 4))


;;;;; Convenience
(define (valid-active-modules? active-modules)
  "Return #t if ACTIVE-MODULES is a valid set of active modules, #f if an
error can be detected."
  (fold (lambda (current previous)
          (if (and previous
                   (pair? current)
                   (blobhash? (car current))  ; minhash
                   (blobhash? (cdr current))) ; fullhash
              #t #f))
        #t active-modules))

(define (no-active-modules? active-modules)
  "Return #t if ACTIVE-MODULES is empty; #f otherwise."
  (null? active-modules))

(define (update-profile field value profile)
  "Return a new profile, based on PROFILE, but augmented by the new VALUE for
FIELD."
  (define (update-if this-field accessor)
    (if (eqv? this-field field) value (accessor profile)))
  (make-profile (update-if 'name           profile-name)
                (update-if 'prof-server    profile-prof-server)
                (update-if 'mod-server     profile-mod-server)
                (update-if 'active-modules profile-active-modules)
                (update-if 'scorecard      profile-scorecard)))

(define* (profile-hash name password)
  (sha256-string name password))

;;; profiles.scm ends here
