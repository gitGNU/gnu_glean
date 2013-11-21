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
  #:use-module (rnrs)
  #:use-module (guilecraft utils)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft scorecard-ops)
  #:use-module (guilecraft whirligigs)
  #:use-module (guilecraft profiler)
  #:use-module (guilecraft problem-type-manager)
  #:use-module (guilecraft gmodule-manager)
  #:use-module (guilecraft gprofile-manager)
  #:use-module (guilecraft data-types sets)

  #:export (generate-challenge
	    generate-evaluation))

;;; Currently returns a new profile only.
;;; portal should return evaluation, as well as correct answer
;;; and  new profile.
(define (check-profile profile)
  ;; TODO: also need to introduce clause that checks to
  ;; make sure the scorecard contains info on all
  ;; active-modules, not just the ones which already happen
  ;; to be there.

  ;; TODO:introduce clause to determine if profile
  ;; scorecard entry order corresponds to order in gmodule,
  ;; to allow for gmodule updates on the fly:
  ;; - if false, change order, or insert new value, continue process
  ;; - if true, continue as at present.

  (define (cleanse-active-modules active-modules)
    (define (cleanser remaining result)
      (cond ((null? remaining)
	     result)
	    (else
	     (let ((current (car remaining)))
	       (if current
		   (cleanser (cdr remaining) (cons current result))
		   (cleanser (cdr remaining) result))))))
    (let ((result (cleanser (map set-id->gmodule-object
				 active-modules) '())))
      (if (null? result)
	  (raise 'no-modules)
	  result)))

  (if (and (profile? profile)
	   (scorecard? (profile-scorecard profile)))
      (let ([name (profile-name profile)]
	    [id (update-id profile)]
	    [active-modules (profile-active-modules profile)])

	;; Check whether scorecard contains data, else generate new
	;; profile with new scorecard
	(cond ((empty-scorecard? (profile-scorecard profile))
	       (make-profile
		name
		id
		active-modules
		(make-scorecard-skeleton
		 (cleanse-active-modules active-modules))))
	      ;; Work in progress:
	      ;; Check whether scorecard is complete, else generate new
	      ;; profile and add missing scorecard data
	      ;; ((complete-scorecard? profile)
	      ;;  (make-profile name id active-modules
	      ;; 		       (complement-scorecard
	      ;; 			(get-active-modules profile))))
	      ;; ENDS :::
	      ;; Otherwise we use the current module
	      (else profile)))
      (assertion-violation
       'check-profile
       "PROFILE is not a profile, or does not contain a scorecard."
       profile (profile-scorecard profile))))

(define (update-profile profile scorecard-object)
  "Take profile and scorecard and return new profile, created using
scorecard."
  (make-profile
   (profile-name profile)
   (profile-id profile)
   (profile-active-modules profile)
   scorecard-object))

;; (define complete-scorecard?
;;   (lambda (profile)
;;     "Recurse through the scorecard, checking if every datum is
;; commensurate with the corresponding active gmodule.

;; We skip all data that exist in the profile but which is not part of
;; an active module - these could be past scores that are currently
;; inactive, and which might be needed in future."
;;     (let ([%active-module-objects (map gman_set-id->gmodule-object
;; 				       %active-modules)]
;; 	  [%scorecard (get-scorecard profile)])
;;       ; Check whether the next datum in the scorecard is the same as
;;       ; the next gset-tag in the gmodule
;;       (cond ((not (eq? next-scorecard-datum next-module-datum))
;; 	     (member? next-scorecard-datum gmodule-object))))))

(define (generate-challenge profile)
  "Return profile and the next challenge object."
  (if (profile? profile)
      (let* ((profile (check-profile profile))
	     (lowest-mod-blob (profiler profile)))
	(if lowest-mod-blob
	    (cons profile
		  (ptm_get-challenge
		   (hangar (mod-blob-hash lowest-mod-blob)
			   (lowest-scoring-gset lowest-mod-blob)
			   (set-blob-hash (lowest-scoring-gset
					 lowest-mod-blob)))))
	    #f))
      (assertion-violation 'generate-challenge
			   "&irritant is not a profile."
			   profile)))

(define (generate-evaluation answer profile)
  "Return updated profile and evaluation result."
  (if (and (profile? profile)
	   answer)
      (let* ((profile (check-profile profile))
	     (lowest-mod-blob (profiler profile)))
	(if lowest-mod-blob
	    (let* ((current-problem
		    (hangar (mod-blob-hash lowest-mod-blob)
			    (lowest-scoring-gset lowest-mod-blob)
			    (mod-blob-hash lowest-mod-blob)))
		   (evaluation-result (ptm_assess-answer answer
							 current-problem)))
	      (llog 	       (update-profile
				profile
				(update-scorecard (profile-scorecard profile)
						  (profiler profile)
						  (lowest-scoring-gset
						   (profiler profile))
						  evaluation-result)))
	      (cons
	       ;; return new profile after score evaluation
	       (update-profile
		profile
		(update-scorecard (profile-scorecard profile)
				  (profiler profile)
				  (lowest-scoring-gset
				   (profiler profile))
				  evaluation-result))
	       ;; append evaluation result to list to be returned.
	       evaluation-result))
	    #f))
      (assertion-violation
       'generate-evaluation
       "PROFILE is not a profile, or ANSWER is #f."
       profile answer)))
