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
;; Portal defines the exposed API through which UIs and other programs
;; access high-level content from guilecraft.
;;
;;; Code:

(define-module (guilecraft portal)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft scorecard-ops)
  #:use-module (guilecraft whirligigs)
  #:use-module (guilecraft profiler)
  #:use-module (guilecraft problem-type-manager)
  #:use-module (guilecraft gmodule-manager)
  #:use-module (guilecraft gprofile-manager)

  #:export (portal
	    make-challenge-request
	    make-eval-request
	    list-profiles
	    select-profile))

(define-record-type <challenge-request>
  (make-challenge-request profile)
  challenge-request?
  (profile challenge-request-profile))

(define-record-type <evaluation-request>
  (make-eval-request answer profile)
  eval-request?
  (answer get-eval-answer)
  (profile eval-request-profile))

(define (request-profile request)
  "Generic procedure to retrieve the profile in a given request. Takes
any request object and returns the profile associated with it."
  (cond ((challenge-request? request)
	 (challenge-request-profile request))
	((eval-request? request)
	 (eval-request-profile request))
	(else (error "request-profile: Unknown Request:" request))))

;;; Currently returns a new profile only.
;;; portal should return evaluation, as well as correct answer
;;; and  new profile.
(define check-profile
	      ; TODO: also need to introduce clause that checks to
	      ; make sure the scorecard contains info on all
	      ; active-modules, not just the ones which already happen
	      ; to be there.

	      ; TODO:introduce clause to determine if profile
	      ; scorecard entry order corresponds to order in gmodule,
	      ; to allow for gmodule updates on the fly:
	      ; - if false, change order, or insert new value, continue process
	      ; - if true, continue as at present.
  (lambda (profile)
    (let ([%name (get-name profile)]
	  [%id (update-id profile)]
	  [%active-modules (get-active-modules profile)])
      ; Check whether scorecard contains data, else generate new
      ; profile with new scorecard
      (cond ((null-gmod-blobs? (scorecard-data (get-scorecard profile)))
	     (make-profile
	      (name %name)
	      (id %id)
	      (active-modules %active-modules)
	      (scorecard (make-scorecard-skeleton
			  (map gmodule-id->gmodule-object %active-modules)))))
	    ;; Work in progress:
	    ; Check whether scorecard is complete, else generate new
	    ; profile and add missing scorecard data
	    ;; ((complete-scorecard? profile)
	    ;;  (make-profile name id active-modules
	    ;; 		       (complement-scorecard
	    ;; 			(get-active-modules profile))))
	    ;; ENDS :::
	    ; Otherwise we use the current module
	    (else profile)))))

(define (update-profile profile scorecard-object)
  "Take profile and scorecard and return new profile, created using
scorecard."
  (make-profile
   (name (get-name profile))
   (id (get-id profile))
   (active-modules (get-active-modules profile))
   (scorecard scorecard-object)))

;; (define complete-scorecard?
;;   (lambda (profile)
;;     "Recurse through the scorecard, checking if every datum is
;; commensurate with the corresponding active gmodule.

;; We skip all data that exist in the profile but which is not part of
;; an active module - these could be past scores that are currently
;; inactive, and which might be needed in future."
;;     (let ([%active-module-objects (map gman_gmodule-id->gmodule-object
;; 				       %active-modules)]
;; 	  [%scorecard (get-scorecard profile)])
;;       ; Check whether the next datum in the scorecard is the same as
;;       ; the next gset-tag in the gmodule
;;       (cond ((not (eq? next-scorecard-datum next-module-datum))
;; 	     (member? next-scorecard-datum gmodule-object))))))

(define portal
  (lambda (request)
    "Returns either the next question for a given profile, or a list
containing the result of the evaluation of the player's answer and the
player's new profile."
    ;; TODO: introduce low-overhead
    ;; profile save, just in case profile in use is replaced with a new
    ;; one below.
    ;; Ensure profile-scorecard is commensurate with
    ;; active-modules
    (let ([profile (check-profile (request-profile request))])
      ;; If challenge request, return new challenge
      (cond ((challenge-request? request)
	     ;; generate challenge
	     (generate-challenge profile))
	    ;; if evaluation request, evaluate and return new profile
	    ;; & evaluation result.
	    ((eval-request? request)
	     (let ([answer (get-eval-answer request)])
	       (generate-evaluation answer profile)))

	    ;; if not challenge or eval, currently unknown request.
	    (else  (error "portal: Unknown Request:" request))))))

(define (generate-challenge profile)
  "Return profile and the next challenge object."
  (cons profile
	(ptm_get-challenge
	 (whirl_hangar 'next
		       (prof_profiler 'get-gset-tag
				      profile)
		       (prof_profiler 'get-gset-gmodule
				      profile)))))

(define (generate-evaluation answer profile)
  "Return  updated profile and evaluation result."
  (let ([current-problem (whirl_hangar 'current
				       (prof_profiler 'get-gset-tag
						      profile)
				       (prof_profiler 'get-gset-gmodule
						      profile))])
    (let ([evaluation-result (ptm_assess-answer answer
						current-problem)])
      (cons
       ;; return new profile after score evaluation
       (update-profile
	profile
	(update-scorecard (get-scorecard profile)
			  (prof_profiler 'get-gset-gmodule
					 profile)
			  (prof_profiler 'get-gset-tag
					 profile)
			  evaluation-result))
       ;; append evaluation result to list to be returned.
       evaluation-result))))

(define (list-profiles)
  "Wrapper pointing to gprofile-manager's list-profiles function."
  (list-gprofiles))

(define (select-profile gprofile-id)
  "Wrapper pointing to gprofile-manager's select-profile function."
  (select-gprofile gprofile-id))
