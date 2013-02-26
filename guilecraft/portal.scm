;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (guilecraft portal)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft gprofiles)	; For profile data-struct
  #:use-module (guilecraft gmodules)	; For whirligig etc.
  #:export (portal))

(define portal
  (lambda (message profile)
    "Returns either the next question for a given profile, or a list
containing the result of the evaluation of the player's answer and the
player's new profile."
    (cond ((and (symbol? message)	; If message is a symbol and 
		(eq? message 'next))	; the symbol next, then we 
					; want a challenge
	   (get-next-challenge 
	    (whirligig-hangar 'next 
			      (profiler 'get-gset-tag profile)
			      (profiler 'get-gset-gmodule profile))))
	  ((string? message)		; If message is a string
					; then we want evaluation
	   (evaluate-current-answer 
	    (whirligig-hangar 'current
			      (profiler 'get-gset-tag profile)
			      (profiler 'get-gset-gmodule profile))))
	  (else 'error-unknown-format)))) ;Unknown format


(define profiler
  (lambda message profile)
  "Returns profile challenge data by evaluating the scores stored in
the profile."
  (cond ((eq? message 'get-gset-tag)
	 ; Return the lowest scoring gset-tag from all active modules
	 (return-lowest-scoring-tag profile))
	((eq? message 'get-gset-gmodule)
	 ; Return gmodule name of lowest scoring gset-tag
	 (return-lowest-scoring-gmodule profile))))

(define return-lowest-scoring-tag
  (lambda (profile)
    "Convenience function to return the lowest scoring tag from a
given profile"
    (return-lowest-scoring-tag-or-gmodule 'get-tag profile)))

(define return-lowest-scoring-gmodule
  (lambda (profile)
    "Convenience function to return the lowest scoring gmodule from a
given profile"
    (return-lowest-scoring-tag-or-gmodule 'get-module profile)))

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
      (lambda (active-modules score-card lowest-scoring-gset)
	(cond ((null? active-modules)
	       'no-active-modules)	; error: no game defined
	      ((null? module-score-data-set)
	       lowest-scoring-gset)	; reached end of profile
					; score-card -> lowest scoring
					; datum
	      ;; next, check if current score card datum is affiliated
	      ;; to an active module, and if so, whether it has a
	      ;; lower score than the currently stored lowest scoring
	      ;; datum
	      ((and (member? (get-module (car score-card)) 
			     (active-modules))
		    (< (get-score (car score-card))
		       (get-score (lowest-scoring-gset))))
	       ;; If so, save current datum, and recurse onwards
	       (helper active-modules 
		       (cdr score-card) 
		       (car score-card)))
	      ;; Else, recurse onwards.
	      (else (helper active-modules 
			    (cdr score-card)
			    lowest-scoring-gset)))))
    ;; Call helper with populated list, and return datum applied to
    ;; proc.
    ;; Proc should retrieve data field in datum, else will cause problems.
    (proc (helper (get-active-modules profile) 
		  (get-score-card profile) 
		  (make-dummy-score-card-datum)))))
