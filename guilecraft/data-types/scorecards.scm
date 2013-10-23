;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types scorecards)
  #:use-module (rnrs records procedural)
  #:export (make-scorecard
	    scorecard?
	    scorecard-data

	    make-mod-blob
	    mod-blob?
	    mod-blob-id
	    mod-blob-data

	    make-set-blob
	    set-blob?
	    set-blob-id
	    set-blob-score
	    set-blob-counter))

(define set-blob-rtd
  (make-record-type-descriptor 'set-blob #f #f #f #f
			       '#((immutable set-id)
				  (immutable score)
				  (immutable counter))))
(define set-blob-rcd
  (make-record-constructor-descriptor set-blob-rtd #f #f))
(define make-set-blob (record-constructor set-blob-rcd))
(define set-blob? (record-predicate set-blob-rtd))
(define set-blob-id (record-accessor set-blob-rtd 0))
(define set-blob-score (record-accessor set-blob-rtd 1))
(define set-blob-counter (record-accessor set-blob-rtd 2))

(define mod-blob-rtd
  (make-record-type-descriptor 'mod-blob #f #f #f #f
			       '#((immutable mod-id)
				  (immutable mod-data))))
(define mod-blob-rcd
  (make-record-constructor-descriptor mod-blob-rtd #f #f))
(define make-mod-blob (record-constructor mod-blob-rcd))
(define mod-blob? (record-predicate mod-blob-rtd))
(define mod-blob-id (record-accessor mod-blob-rtd 0))
(define mod-blob-data (record-accessor mod-blob-rtd 1))

(define scorecard-rtd
  (make-record-type-descriptor 'scorecard #f #f #f #f
			       '#((immutable data))))
(define scorecard-rcd
  (make-record-constructor-descriptor scorecard-rtd #f #f))
(define make-scorecard (record-constructor scorecard-rcd))
(define scorecard? (record-predicate scorecard-rtd))
(define scorecard-data (record-accessor scorecard-rtd 0))
