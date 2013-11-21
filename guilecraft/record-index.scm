;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;; Copyright (C) 2008, 2010, 2012 Alex Sassmannshausen

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

;;; Commentary:
;;
;; Module providing a list of dummy records for use with register-rtds
;;
;;; Code:

(define-module (guilecraft record-index)
  #:use-module (rnrs records procedural)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft data-types module-requests)
  #:use-module (guilecraft data-types profile-requests)
  #:export (get-rc
	    known-rc?))

(define (get-rc rec-name)
  "Syntactic sugar. Populate cache if necessary and get REC-NAME's
constructor."
  (assv-ref record-kv-pairs rec-name))

(define (known-rc? rec-name)
  "Syntactic sugar. Return false if rec-name is not yet known by
known-rcs."
  (if (get-rc rec-name)
      #t
      #f))

(define record-kv-pairs
  `((set . ,mecha-set)
    (question . ,make-q)
    (solution . ,make-s)
    (option . ,make-o)
    (medii . ,make-media)
    (prob . ,make-problem)
    (predicate . ,p)

    (scorecard . ,make-scorecard)
    (blob . ,make-blob)

    (profile . ,make-profile)
    (id . ,make-id)

    ;; generic requests
    (request . ,request)
    (response . ,response)

    (alive-rq . ,alive-rq)
    (quit-rq . ,quit-rq)
    (ack-rs . ,ack-rs)
    (neg-rs . ,neg-rs)
    (unk-rs . ,unk-rs)

    ;; profile requests
    (reg-rq . ,reg-rq)
    (modq . ,modq)
    (mods . ,mods)
    (set!q . ,set!q)
    (auth-rq . ,auth-rq)
    (chauth-rq . ,chauth-rq)
    (chauth-rs . ,chauth-rs)
    (evauth-rq . ,evauth-rq)
    (auth-rs . ,auth-rs)

    ;; module requests
    (chall-rq . ,chall-rq)
    (chall-rs . ,chall-rs)
    (eval-rq . ,eval-rq)
    (eval-rs . ,eval-rs)
    (hashmapq . ,hashmapq)
    (hashmaps . ,hashmaps)
    (hash-rq . ,hash-rq)))
