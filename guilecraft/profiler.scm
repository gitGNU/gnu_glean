;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft profiler)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types gprofiles)	; For profile data-struct
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft scorecard-ops)

  #:export (prof_profiler))

;;; Commentary:
;;
;; Profiler provides an algorithm to retrieve the next eligible
;; candidate problem by assessing the player's profile.
;;
;; Currently it simply returns the lowest scoring gset-tag or
;; gmodule-id of the gmodule containing the lowest scoring gset.
;;
;;; Code:

(define prof_profiler
  (lambda (message profile)
    "Returns profile challenge data by evaluating the scores stored in
the profile."
    (cond ((eq? message 'get-gset-tag)
					; Return the lowest scoring gset-tag from all active modules
	   (return-lowest-scoring-tag profile))
	  ((eq? message 'get-gset-gmodule)
					; Return gmodule name of lowest scoring gset-tag
	   (return-lowest-scoring-gmodule profile))
	  (else (error "prof_profiler: unknown message: " message)))))

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

(define return-lowest-scoring-tag-or-gmodule
  (lambda (proc profile)
    "Returns/extracts the lowest scoring tag or gmodule from the given
profile"
    (define helper  			; recurse through profile
					; returning lowest scoring data
      (lambda (active-modules scorecard
			      lowest-scoring-scorecard-datum)
	(cond ((gprof_empty-active-modules? active-modules)
	       (error "return-lowest-scoring-tag-or-gmodule: Profile
contains no active modules: " profile))
	      ((scc_empty-scorecard? scorecard)
	       lowest-scoring-scorecard-datum)	; reached end of profile
					; scorecard -> lowest scoring
					; datum
	      ;; otherwise, check if current score card datum is affiliated
	      ;; to an active module, and if so, whether it has a
	      ;; lower score than the currently stored lowest scoring
	      ;; datum
	      ((and (active-module? (scc_first-in-scorecard scorecard)
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
