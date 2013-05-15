;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types scorecards)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-scorecard
	    scorecard?
	    scorecard-data

	    make-gmod-blob
	    gmod-blob?
	    gmod-blob-id
	    gmod-blob-data

	    make-gset-blob
	    gset-blob?
	    gset-blob-tag
	    gset-blob-score))

(define-record-type <score-gset-blob>
  (make-gset-blob set-tag score)
  gset-blob?
  (set-tag gset-blob-tag)
  (score gset-blob-score))

(define-record-type <score-gmod-blob>
  (make-gmod-blob gmodule-id gset-data)
  gmod-blob?
  (gmodule-id gmod-blob-id)
  (gset-data gmod-blob-data))

(define-record-type <scorecard>
  (make-scorecard data)
  scorecard?
  (data scorecard-data))
