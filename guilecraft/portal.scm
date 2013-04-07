;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (guilecraft portal)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft gprofiles)	; For profile data-struct
  #:use-module (guilecraft gmodules)	; For whirligig etc.
  #:use-module (guilecraft whirligigs)
  #:use-module (guilecraft scorecards)
  
  #:use-module (guilecraft open-problems)
  #:export (port_portal
	    port_make-challenge-request
	    port_make-eval-request
	    port_challenge-request?))

(define-record-type <challenge-request>
  (port_make-challenge-request profile)
  port_challenge-request?
  (profile get-challenge-profile))

(define-record-type <evaluation-request>
  (port_make-eval-request answer profile)
  eval-request?
  (answer get-eval-answer)
  (profile get-eval-profile))

;;; 
;; Helper functions
;;; 

(define profiler
  (lambda (message profile)
    "Returns profile challenge data by evaluating the scores stored in
the profile."
    (cond ((eq? message 'get-gset-tag)
					; Return the lowest scoring gset-tag from all active modules
	   (return-lowest-scoring-tag profile))
	  ((eq? message 'get-gset-gmodule)
					; Return gmodule name of lowest scoring gset-tag
	   (return-lowest-scoring-gmodule profile))
	  (else (error "profiler: unknown message: " message)))))

(define return-lowest-scoring-tag
  (lambda (profile)
    "Convenience function to return the lowest scoring tag from a
given profile"
    (return-lowest-scoring-tag-or-gmodule scc_get-scorecard-datum-gset-tag profile)))

(define return-lowest-scoring-gmodule
  (lambda (profile)
    "Convenience function to return the lowest scoring gmodule from a
given profile"
    (return-lowest-scoring-tag-or-gmodule scc_get-scorecard-datum-gmodule-id profile)))

;; Function below carries out meat of computation. It relies on
;; properly exposed data structures defined in the gprofiles module,
;; to access the data stored within it.
;; I need to define those data structures next.

(define return-lowest-scoring-tag-or-gmodule
  (lambda (proc profile)
    "Returns/extracts the lowest scoring tag or gmodule from the given
profile"
    (define helper  			; recurse through profile
					; returning lowest scoring data
      (lambda (active-modules scorecard lowest-scoring-scorecard-datum)
	(cond ((gprof_empty-active-modules? active-modules)
	       'no-active-modules)	; error: no game defined
	      ((scc_end-of-scorecard? scorecard)
	       lowest-scoring-scorecard-datum)	; reached end of profile
					; scorecard -> lowest scoring
					; datum
	      ;; next, check if current score card datum is affiliated
	      ;; to an active module, and if so, whether it has a
	      ;; lower score than the currently stored lowest scoring
	      ;; datum
	      ((and (gprof_active-module? (scc_first-in-scorecard scorecard) 
				    active-modules)
		    (scc_lower-score? (scc_first-in-scorecard scorecard)
				  lowest-scoring-scorecard-datum))
	       ;; If so, save current datum, and recurse onwards
	       (helper active-modules
		       (scc_rest-of-scorecard scorecard)
		       (scc_first-in-scorecard scorecard)))
	      ;; Else, recurse onwards.
	      (else (helper active-modules 
			    (scc_rest-of-scorecard scorecard)
			    lowest-scoring-scorecard-datum)))))
    ;; Call helper with populated list, and return datum applied to
    ;; proc.
    ;; Proc should retrieve data field in datum, else will cause problems.
    (proc (helper (gprof_get-active-modules profile) 
		  (gprof_get-scorecard profile) 
		  (scc_make-dummy-scorecard-datum)))))

;;; 
;; High Level Evaluate Answer
;;; 

(define assess-answer
  (lambda (player-answer challenge)
    "Currently returns #t if @var{player-answer} is assessed
successfully against @var{whirligig}'s current problem's solution.
This is a high-level function, called directly from the UI."
    (cond ((op_open-problem? challenge)
	   (op_assess player-answer 
		      (op_get-solution challenge)))
	  (else 'unknown-problem-type))))

(define get-challenge
  (lambda (challenge)
    "Return a challenge, depending on the type of problem."
    (cond ((op_open-problem? challenge)
	   (op_get-challenge challenge))
	  (else 'unknown-problem-type))))

;;; Currently is spaghetti code - probably needs to be split into
;;; separate functions and might need to be exported into a separate
;;; module. Currently returns a new profile only., which then makes
;;; port_portal return the new profile. port_portal should return
;;; evaluation, as well as correct answer and new profile.

(define update-profile
  (lambda (old-profile assessment-result)
    "Return a new profile, which is used to generate the next
challenge on the basis of the existing profile and a boolean, normally
derived from player answer assessment."
    (let ([lowest-scoring-datum `(,(return-lowest-scoring-tag
				   old-profile)
				  ,(return-lowest-scoring-gmodule old-profile))])
	(define helper
	  (lambda (temp-scorecard lowest-scoring-datum
				  new-scorecard)
	    "Return a modified scorecard by recursing through tho old
scorecard and updating the lowest scoring datum"
	    (cond ((null? temp-scorecard)
		   (reverse new-scorecard))
		  ((and (eq? (scc_get-scorecard-datum-gmodule-id
			      (scc_first-in-scorecard temp-scorecard))
			     (cadr lowest-scoring-datum)))
		   (helper (scc_rest-of-scorecard temp-scorecard)
			   lowest-scoring-datum (cons
						 (scc_make-scorecard-datum
						  (scc_get-scorecard-datum-gmodule-id
						   (scc_first-in-scorecard
						    temp-scorecard))
						  (scc_get-scorecard-datum-gset-tag
						   (scc_first-in-scorecard
						    temp-scorecard))
						  (+ (scc_get-scorecard-datum-score
						      (scc_first-in-scorecard
						       temp-scorecard))
						     1)) 
				 new-scorecard)))
		  (else (helper (scc_rest-of-scorecard temp-scorecard)
				lowest-scoring-datum 
				(cons (scc_first-in-scorecard
				       temp-scorecard)
				      new-scorecard))))))

	(cond ((eq? assessment-result #t) 
	       (gprof_make-profile (gprof_get-UUID old-profile)
				   (gprof_get-name old-profile)
				   (gprof_get-active-modules old-profile)
				   (helper (gprof_get-scorecard old-profile)
					   lowest-scoring-datum
					   '())))
	      (else old-profile)))))


;;; 
;; High Level UI Entry Point
;;; 
;;; Currently returns a new profile only.
;;; port_portal should return evaluation, as well as correct answer
;;; and  new profile.

(define port_portal
  (lambda (request)
    "Returns either the next question for a given profile, or a list
containing the result of the evaluation of the player's answer and the
player's new profile."
    (catch #t 
      (lambda ()
	(cond ((port_challenge-request? request)
	       (get-challenge
		(whirl_hangar 'next 
			      (profiler 'get-gset-tag 
					(get-challenge-profile request))
			      (profiler 'get-gset-gmodule 
					(get-challenge-profile request)))))
	      ((eval-request? request)
	       (update-profile (get-eval-profile request)
			       (assess-answer 
				(get-eval-answer request)
				(whirl_hangar 'current
					      (profiler 'get-gset-tag  
							(get-eval-profile request))
					      (profiler 'get-gset-gmodule
							(get-eval-profile request))))))
	      (else (error "portal: Unknown Request:" request))))
      (lambda (k . args)
	(format (current-output-port) "'~a: ~a\n" k args))))) ;Unknown format

