;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types scorecards)
  #:use-module (rnrs records procedural)
  #:export (make-scorecard
	    scorecard?
	    scorecard-data

	    make-blob
	    blob?
	    blob-hash
	    blob-parents
	    blob-children
	    blob-score
	    blob-counter))

(define blob-rtd
  (make-record-type-descriptor 'blob #f #f #f #f
			       '#((immutable hash)
				  (immutable parents)
				  (immutable children)
				  (immutable score)
				  (immutable counter))))
(define blob-rcd
  (make-record-constructor-descriptor blob-rtd #f #f))
(define make-blob (record-constructor blob-rcd))
(define blob? (record-predicate blob-rtd))
(define blob-hash (record-accessor blob-rtd 0))
(define blob-parents (record-accessor blob-rtd 1))
(define blob-children (record-accessor blob-rtd 2))
(define blob-score (record-accessor blob-rtd 3))
(define blob-counter (record-accessor blob-rtd 4))

(define scorecard-rtd
  (make-record-type-descriptor 'scorecard #f #f #f #f
			       '#((immutable data))))
(define scorecard-rcd
  (make-record-constructor-descriptor scorecard-rtd #f #f))
(define make-scorecard (record-constructor scorecard-rcd))
(define scorecard? (record-predicate scorecard-rtd))
(define scorecard-data (record-accessor scorecard-rtd 0))
