;;; guilecraft --- user profile data-types.         -*- coding: utf-8 -*-

(define-module (guilecraft scorecards)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (scc_make-scorecard-datum
	    scc_get-scorecard-datum-gset-tag
	    scc_get-scorecard-datum-gmodule-name
	    scc_make-dummy-scorecard-datum

	    scc_first-in-scorecard
	    scc_rest-of-scorecard
	    scc_end-of-scorecard?

	    scc_lower-score?))

(define-record-type <scorecard-datum>
  (scc_make-scorecard-datum module set-tag score)
  scorecard-datum?
  (module scc_get-scorecard-datum-gmodule-name)
  (set-tag scc_get-scorecard-datum-gset-tag)
  (score get-scorecard-datum-score))

(define scc_make-dummy-scorecard-datum
  (lambda ()
    (scc_make-scorecard-datum 'no-module 'no-set-tag #f)))

(define dummy-scorecard-datum?
  (lambda (scorecard-datum)
    (if (and (eq? (scc_get-scorecard-datum-gmodule-name scorecard-datum)
		  'no-module)
	     (eq? (scc_get-scorecard-datum-gset-tag scorecard-datum)
		  'no-set-tag)
	     (not (get-scorecard-datum-score scorecard-datum)))
	#t
	#f)))

(define scc_first-in-scorecard
  (lambda (scorecard)
    (car scorecard)))

(define scc_rest-of-scorecard
  (lambda (scorecard)
    (cdr scorecard)))

(define scc_end-of-scorecard?
  (lambda (scorecard)
    (null? scorecard)))

(define scc_lower-score?
  (lambda (scorecard-datum1 scorecard-datum2)
    (cond ((dummy-scorecard-datum? scorecard-datum2)
	   #t)
	  ((< (get-scorecard-datum-score scorecard-datum1) 
	      (get-scorecard-datum-score scorecard-datum2))
	   #t)
	  (else #f))))
