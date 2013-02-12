;;; guilecraft --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of guilecraft.
;;;
;;; guilecraft is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; guilecraft is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guilecraft.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guilecraft gmodules)
  ;; #:use-module (guix utils)
  ;; #:use-module (guix store)
  ;; #:use-module (guix build-system)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-gmodule
            gmodule?
            gmodule-name
            gmodule-version
            gmodule-full-name
            gmodule-description
            gmodule-long-description
            gmodule-license))

;;; Commentary:
;;;
;;; This module provides a high-level mechanism to define gmodules to 
;;; be used in guilecraft.
;;;
;;; Code:


;; A gmodule.
;; Gmodules consist of meta-data and one or more sets/gmodules
(define-record-type <gmodule>
  (make-gmodule name version description long-description creators find-out-more derivation-source parts)
  gmodule?
  ;; What's the gmodule called?
  (name get-name)                   ; string
  ;; Version should really be a number
  (version get-version)             ; number
  ;; First introduction for users
  (description get-description)     ; one-line descriptive string
  ;; Tutorial & background
  (long-description get-long-description) ; Space for teaching material text
  ;; Credit for work done
  (creators get-creators)           ; <credit> instance
  ;; Further Reading
  (find-out-more get-find-out-more) ; <materials> instance
  ;; Give a shout out to your inspiration
  (derivation-source get-derivation-source) ; <materials> instance
  ;; 'Contents' of your gmodule: 'external' gmodules and 'internal' sets
  (parts get-parts)                 ; list of variables
)

;; (set-record-type-printer! 
;;  <gmodule>
;;  (lambda (gmodule port)
;;    (format port "#<gmodule ~a-~a ~a:~a ~a>"
;;            (get-name gmodule)
;;            (get-version gmodule)
;;            (get-description gmodule))))

(define (gmodule-full-name gmodule)
  "Return the full name of GMODULE--i.e., `NAME-VERSION'."
  (string-append (get-name gmodule) "-" (get-version gmodule)))


;; Accreditation: Give creds where they are due
(define-record-type <credit>
  (make-credit current-creator past-creator contributor)
  credit?
  ;; The current maintainer of the module
  (current-creator get-current-creator)           ; string
  ;; Honor past work
  (past-creator    get-past-creator)              ; string
  ;; regular contributor
  (contributor     get-contributor))              ; string

;; Background Information: materials for self-directed
;; study or reference to origin of module.
(define-record-type <materials>
  (make-materials identity reading video audio venue)
  materials?
  ;; Organization, group or person associated with resource
  (identity get-identity)
  ;; Reading: websites, books, pamphlet, etc.
  (reading  get-reading)
  (video    get-video)
  (audio    get-audio)
  ;; Regular haunts: meetups, #channels, etc.
  (venue    get-venue))

;;; 
;; Define gsets
;;;

;;;
;; Gsets are a means of grouping problems together under a
;; tag. Guilecraft uses the tag to measure progress against a
;; discipline, and will select a "random" question from the
;; algorithmically determined highest priority gset. The overall
;; topography that should be starting to manifest is one where
;; disciplines are provided by gmodules, which either point to other
;; gmodules (to make use of content that has already been created), or
;; which contain a number of gsets that then allow guilecraft to
;; select problems.

;; Gset: a group of problems to be used as stream source of challenges
(define-record-type <gset>
  (make-gset tag problems)
  gset?
  ;; Tag: the problem subject ID against which Guilecraft
  ;; will keep 'score'
  (tag get-tag)             ; symbol
  ;; Problems: this is a list of problems, generated through 
  ;; the problems interfaces.
  (problems get-problems)             ; list of <problems>
  )

;;;
;; Define Problems
;;;

;;;
;; Problems are the lowest level data-type used in gmodule definition.
;; They provide a challenge and some form of correct solution.
;; We aim to provide a great variety of problem-types such as:
;; - open-problems: challenges expecting a freely typed answer that 
;; matches the provided solution
;; - multiple-choice: challenges that offer a number of solutions from
;; which the user chooses.
;; - dynamic: challenges are generated dynamically according to rules,
;; the user must type an answer that matches the algorithmically
;; generated solution.
;;
;; Problems should also be able to provide their challenges using
;; different media types: text, audio, video.


;; Open-problem: an individual problem record of the open problem
;; variety.
(define-record-type <open-problem>
  (make-open-problem challenge solution)
  open-problem?
  ; Challenges are posed to players to provide a solution to.
  (challenge get-challenge)      ; String
  ; Solutions are what the player's response is assessed against.
  (solution get-solution)        ; String 
  ) 

;; Helper Functions:
(define assess-open-problem?
  (lambda (player-answer open-problem-solution)
    "Evaluate @var{player-answer} against @var{open-problem-solution}
and return #t if correct or #f if incorrect."
    (if (eq-open-problem? player-answer open-problem-solution)
	#t
	#f)))

(define eq-open-problem? 
  (lambda (player-answer open-problem-solution)
    "Predicate equivalence for @var{player-answer} and
@var{open-problem-solution}. This is separated into a separate module
to maximise the resilience of the superstructure against low-level changes."
    (if (equal? player-answer open-problem-solution)
	#t
	#f)))
