;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft scorecard-ops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft gmodule-ops)
  #:use-module (guilecraft data-manager)
  #:use-module (guilecraft profiler)
  #:export (scc_make-dummy-scorecard-datum
	    scc_dummy-scorecard-datum?
	    active-module?

	    scc_first-in-scorecard
	    scc_rest-of-scorecard
	    scc_empty-scorecard?
	    scc_update-scorecard

	    scc_lower-score?

	    scc_make-scorecard-skeleton))

(define scc_make-scorecard-skeleton
  (lambda (active-modules)
    "Take a list of module-ids and return a new scorecard with a
complete set of data and scores set to 0.

Scorecards are a list, containing a scorecard-datum for each gset-tag,
for each acive-module in the active-modules list."
    (cond
     ((null? active-modules)
      '())
     (else
      (append
       (add-scorecard-data (car active-modules)
			   (gmod_get-gmodule-tags
			    (dman_gmodule-id->gmodule-object
			     (car active-modules))))
       (scc_make-scorecard-skeleton (cdr active-modules)))))))

(define add-scorecard-data
  (lambda (active-module-id gset-tags)
    (cond ((null? gset-tags)
	   '())
	  (else (cons (add-scorecard-datum active-module-id
					   (car gset-tags))
		      (add-scorecard-data active-module-id
					  (cdr gset-tags)))))))

(define add-scorecard-datum
  (lambda (module-id set-tag)
    (scc_make-scorecard-datum module-id set-tag 0)))

(define scc_make-dummy-scorecard-datum
  (lambda ()
    (scc_make-scorecard-datum 'no-module 'no-set-tag #f)))

(define scc_dummy-scorecard-datum?
  (lambda (scorecard-datum)
    (if (and (eq? (scc_get-scorecard-datum-gmodule-id scorecard-datum)
		  'no-module)
	     (eq? (scc_get-scorecard-datum-gset-tag scorecard-datum)
		  'no-set-tag)
	     (not (scc_get-scorecard-datum-score scorecard-datum)))
	#t
	#f)))

(define scc_first-in-scorecard
  (lambda (scorecard)
    (car scorecard)))

(define scc_rest-of-scorecard
  (lambda (scorecard)
    (cdr scorecard)))

(define scc_empty-scorecard?
  (lambda (scorecard)
    (null? scorecard)))

(define scc_lower-score?
  (lambda (scorecard-datum1 scorecard-datum2)
    (cond ((scc_dummy-scorecard-datum? scorecard-datum2)
	   #t)
	  ((< (scc_get-scorecard-datum-score scorecard-datum1)
	      (scc_get-scorecard-datum-score scorecard-datum2))
	   #t)
	  (else #f))))

;;; Currently is spaghetti code - probably needs to be split into
;;; separate functions and might need to be exported into a separate
;;; module. Currently returns a new profile only., which then makes
;;; port_portal return the new profile. port_portal should return
;;; evaluation, as well as correct answer and new profile.

(define scc_update-scorecard
  (lambda (old-profile assessment-result)
    "Return a new profile, which is used to generate the next
challenge on the basis of the existing profile and a boolean, normally
derived from player answer assessment."
    (let ([lowest-scoring-datum
	   (scc_make-scorecard-datum
	    (prof_profiler 'get-gset-gmodule old-profile)
	    (prof_profiler 'get-gset-tag old-profile)
	    0)])
	(define helper
	  (lambda (temp-scorecard lowest-scoring-datum
				  new-scorecard)
	    "Return a modified scorecard by recursing through tho old
scorecard and updating the lowest scoring datum"
	    (cond ((null? temp-scorecard)
		   (reverse new-scorecard))
		  ((and (eq? (scc_get-scorecard-datum-gset-tag
			      (scc_first-in-scorecard temp-scorecard))
			     (scc_get-scorecard-datum-gset-tag lowest-scoring-datum)))
		   (helper (scc_rest-of-scorecard temp-scorecard)
			   lowest-scoring-datum
			   (cons
			    (scc_make-scorecard-datum
			     (scc_get-scorecard-datum-gmodule-id
			      (scc_first-in-scorecard temp-scorecard))
			     (scc_get-scorecard-datum-gset-tag
			      (scc_first-in-scorecard temp-scorecard))
			     (+ (scc_get-scorecard-datum-score
				 (scc_first-in-scorecard temp-scorecard))
				1))
			    new-scorecard)))
		  (else (helper (scc_rest-of-scorecard temp-scorecard)
				lowest-scoring-datum
				(cons (scc_first-in-scorecard
				       temp-scorecard)
				      new-scorecard))))))

	(cond ((eq? assessment-result #t)
	       (let ([%name (gprof_get-name old-profile)]
		     [%id (gprof_update-id old-profile)]
		     [%active-modules (gprof_get-active-modules old-profile)])
		 (gprof_make-profile
		  (name %name)
		  (id %id)
		  (active-modules %active-modules)
		  (scorecard (helper (gprof_get-scorecard old-profile)
				     lowest-scoring-datum
				     '())))))
	      (else old-profile)))))

(define active-module?
  (lambda (scorecard-datum active-modules)
    (member (scc_get-scorecard-datum-gmodule-id scorecard-datum)
	    active-modules)))
