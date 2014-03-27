;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types scorecards)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft data-manager)
  #:use-module (guilecraft utils)
  #:use-module (rnrs)
  #:export (make-scorecard
	    scorecard?
	    scorecard-data

	    make-blob
	    blob?
	    blob-hash
	    blob-parents
	    blob-children
	    blob-score
	    blob-counter

	    make-empty-scorecard
	    empty-scorecard?
	    extend-scorecard

	    scorecard-first
	    scorecard-rest

	    make-dummy-blob
	    dummy-blob?
	    find-blob
	    check-blob
	    add-blob
	    add-blobs

	    update-scorecard

	    active-module?
	    lower-score?
	    blobhash?

	    make-scorecard-skeleton))

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
  (make-record-type-descriptor 'score-card #f #f #f #f
			       '#((immutable data))))
(define scorecard-rcd
  (make-record-constructor-descriptor scorecard-rtd #f #f))
(define make-scorecard (record-constructor scorecard-rcd))
(define scorecard? (record-predicate scorecard-rtd))
(define scorecard-data (record-accessor scorecard-rtd 0))

;;;; Scorecard Operations
(define (blobhash? obj) (symbol? obj))

(define (find-blob blobhash scorecard)
  "Return the blob stored under BLOBHASH in SCORECARD."
  (let ((data (scorecard-data scorecard)))
    (define (list-blobs)
      (data 'values))
    (clog "printing all blobs")
    (rprinter (list-blobs))
    (rprinter (data 'get blobhash))
    ((scorecard-data scorecard) 'get blobhash)))

(define (check-blob blobhash scorecard)
  "Return #t if SCORECARD contains BLOBHASH, otherwise #f."
  ((scorecard-data scorecard) 'contains blobhash))

(define (add-blob blob scorecard)
  "Return BLOB if we have been able to add BLOB, indexed by its
blobhash, in SCORECARD. Otherwise return #f."
  (let ((result ((scorecard-data scorecard)
		 'put (blob-hash blob) blob)))
    (if result result #f)))
(define (add-blobs blobs scorecard)
  "Return a new scorecard created by adding BLOBS to SCORECARD."
  (let ((data (scorecard-data scorecard)))
    (map (lambda (blob) (data 'put (blob-hash blob) blob))
	 blobs)
    (make-scorecard data)))

(define (make-empty-scorecard)
  "Return a scorecard with no scorecard-data."
  (make-scorecard (data-manager blob?)))

(define (empty-scorecard? scorecard)
  "Return #t if scorecard has no data."
  (null? (vector->list ((scorecard-data scorecard) 'list-keys))))

(define (make-dummy-blob)
  "Return a score-blob with no real data."
  (make-blob 'no-tag '() '() #f 0))

(define (dummy-blob? score-blob)
  "Return #t if score-mod-blob is a dummy-blob."
  (if (and (eq? (blob-hash score-blob) 'no-tag)
	   (not (blob-score score-blob))
	   (zero? (blob-counter score-blob)))
      #t
      #f))

;; ;; Data abstraction for processing gset blobs.
;; (define (scorecard-first scorecard)
;;   "Return the first blob within blobs."
;;   (car (scorecard-data scorecard)))
;; (define (scorecard-rest scorecard)
;;   "Return the rest of the blobs in blobs."
;;   (cdr (scorecard-data scorecard)))

(define (lower-score? blob1 blob2)
  "Return #t if blob1 is dummy-blob, or if its score is
lower than that of blob1."
  (cond ((not (and (blob? blob2) (blob? blob1)))
	 (error 'lower-score? "blob1 or blob2 not a blob."))
	((dummy-blob? blob1) #f) ;; dummy blob is always larger
	((> (blob-score blob1)
	    (blob-score blob2))
	 #f) ;; if blob1 is larger; false.
	(else #t)))

(define (active-module? crown-blob active-modules)
  "Return #t if the blob-hash in crown-blob is part of the
active-modules list."
  (member (blob-hash crown-blob)
	  active-modules))

(define (extend-scorecard blobs scorecard)
  "Return a new scorecard by merging BLOBS into SCORECARD."
  (define (fold-blobs result-so-far blob)
    (if (and result-so-far (add-blob blob scorecard)) #t #f))
  (fold-left fold-blobs #t blobs))

(define (update-scorecard scorecard blobhash assessment-result)
  "Return a new scorecard, on the basis of SCORECARD, with the score
of the relevant blob adjusted based on ASSESSMENT-RESULT and
BLOBHASH."
  (let ((new-blob (update-blob ((scorecard-data scorecard) 'get
                                blobhash) assessment-result))
        (sc-data (scorecard-data scorecard)))
    (sc-data 'put blobhash new-blob)))

(define (update-blob blob assessment-result)
  "Return a new blob constructed on the basis of BLOB, with its score
adapted according to ASSESSMENT-RESULT and its counter progressed."
  (make-blob (blob-hash blob)
	     (blob-parents blob)
	     (blob-children blob)
             (modify-score (blob-score blob)
                           assessment-result)
             (progress-counter (blob-counter blob)
                               assessment-result)))

(define (modify-score old-score assessment-result)
  "Returns a score modified by an algorithm on the basis of
assessment-result, to take the place of old-score."
  (if assessment-result
      (1+ old-score)
      old-score))

(define (progress-counter old-counter-value assessment-result)
  "Returns the increment of counter-value or 0 if counter-value
is equal to or greater the number of problems in the gset."
  (1+ old-counter-value))
