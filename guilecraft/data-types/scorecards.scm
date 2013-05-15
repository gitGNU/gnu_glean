;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types scorecards)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-scorecard
	    scorecard?
	    scorecard-data

	    make-score-gmod-blob
	    score-gmod-blob?
	    score-gmod-blob-id
	    score-gmod-blob-data

	    make-score-gset-blob
	    score-gset-blob?
	    score-gset-blob-tag
	    score-gset-blob-score))

(define-record-type <score-gset-blob>
  (make-score-gset-blob set-tag score)
  score-gset-blob?
  (set-tag score-gset-blob-tag)
  (score score-gset-blob-score))

(define-record-type <score-gmod-blob>
  (make-score-gmod-blob gmodule-id gset-data)
  score-gmod-blob?
  (gmodule-id score-gmod-blob-id)
  (gset-data score-gmod-blob-data))

(define-record-type <scorecard>
  (make-scorecard data)
  scorecard?
  (data scorecard-data))
