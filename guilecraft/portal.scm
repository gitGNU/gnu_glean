;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (guilecraft portal)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft gprofiles)	; For profile data-struct
  #:use-module (guilecraft gmodules)	; For whirligig etc.
  #:use-module (guilecraft whirligigs)
  #:use-module (guilecraft scorecards)
  
  #:use-module (guilecraft open-problems)
  #:export (port_portal))

;;; 
;; High Level UI Entry Point
;;; 

(define port_portal
  (lambda (message profile)
    "Returns either the next question for a given profile, or a list
containing the result of the evaluation of the player's answer and the
player's new profile."
    (cond ((and (symbol? message)	; If message is a symbol and 
		(eq? message 'next))	; the symbol next, then we 
					; want a challenge
	   (op_get-challenge
	    (whirl_hangar 'next 
			      (profiler 'get-gset-tag profile)
			      (profiler 'get-gset-gmodule profile))))
	  ((string? message)		; If message is a string
					; then we want evaluation
	   (assess-answer 
	    message
	    (whirl_hangar 'current
			      (profiler 'get-gset-tag profile)
			      (profiler 'get-gset-gmodule profile))))
	  (else 'error-unknown-format)))) ;Unknown format

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
	   (return-lowest-scoring-gmodule profile)))))

(define return-lowest-scoring-tag
  (lambda (profile)
    "Convenience function to return the lowest scoring tag from a
given profile"
    (return-lowest-scoring-tag-or-gmodule scc_get-scorecard-datum-gset-tag profile)))

(define return-lowest-scoring-gmodule
  (lambda (profile)
    "Convenience function to return the lowest scoring gmodule from a
given profile"
    (return-lowest-scoring-tag-or-gmodule scc_get-scorecard-datum-gmodule-name profile)))

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
	  (else 'not-working))))