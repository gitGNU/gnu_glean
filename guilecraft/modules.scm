;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guilecraft gmodules)
  ;; #:use-module (guix utils)
  ;; #:use-module (guix store)
  ;; #:use-module (guix build-system)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:export (gmodule
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

;; A gmodule.
;; Gmodules consist of meta-data and one or more sets/gmodules
(define-record-type <gmodule>
  (make-gmodule name version description long-description creators find-out-more derivation-source parts license)
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
  (license gmodule-license (default '()))) ; This will default to CC-by-SA

(set-record-type-printer! 
 <gmodule>
 (lambda (gmodule port)
   (format port "#<gmodule ~a-~a ~a:~a ~a>"
           (get-name gmodule)
           (get-version gmodule)
           (get-description gmodule))))

(define (gmodule-full-name gmodule)
  "Return the full name of GMODULE--i.e., `NAME-VERSION'."
  (string-append (get-name gmodule) "-" (get-version gmodule)))
