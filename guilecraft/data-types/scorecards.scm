;;; guilecraft --- user profile data-types.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types scorecards)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (scc_make-scorecard-datum
	    scc_get-scorecard-datum-gset-tag
	    scc_get-scorecard-datum-gmodule-id
	    scc_get-scorecard-datum-score))

(define-record-type <scorecard-datum>
  (scc_make-scorecard-datum module-id set-tag score)
  scorecard-datum?
  (module-id scc_get-scorecard-datum-gmodule-id)
  (set-tag scc_get-scorecard-datum-gset-tag)
  (score scc_get-scorecard-datum-score))
