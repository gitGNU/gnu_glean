;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft scorecard-ops)
  #:use-module (rnrs)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft gset-ops)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft gmodule-ops)
  #:use-module (guilecraft data-manager)
  #:export (make-empty-scorecard
	    empty-scorecard?
	    extend-scorecard

	    scorecard-first
	    scorecard-rest

	    make-dummy-blob
	    dummy-blob?
	    check-blob
	    add-blob

	    update-scorecard

	    active-module?

	    lower-score?

	    make-scorecard-skeleton))

(define (check-blob blobhash scorecard)
  "Return #t if SCORECARD contains BLOBHASH, otherwise #f."
  ((scorecard-data scorecard) 'contains blobhash))

(define (add-blob blob scorecard)
  "Return BLOB if we have been able to add BLOB, indexed by its
blobhash, in SCORECARD. Otherwise return #f."
  (let ((result ((scorecard-data scorecard)
		 'put (blob-hash blob) blob)))
    (if result result #f)))

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
  "Return #t if blob2 is dummy-blob, or if its score is
lower than that of blob1."
  (cond ((dummy-blob? blob2)
	 #t)
	((< (blob-score blob1)
	    (blob-score blob2))
	 #t)
	(else #f)))

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

(define (make-scorecard-skeleton gmodule-object-list)
  "Return a scorecard populated with a score-mod-blob for each
gmodule-object, in turn containing a nil-score-blob for each
gset-tag in that gmodule-objects' gmodule-parts section."

  (define (add-scorecard-data gmodule-object-list)
    "Return a list of mod-blobs containing nil-score-blobs for
each gmodule-object in the list."
    (map add-nil-score-mod-blob gmodule-object-list))

  (define (add-nil-score-mod-blob gmodule-object)
    "Return a mod-blob containing a nil-score blob for each
gset-tag in the gmodule object."
    (make-mod-blob (set-id gmodule-object)
		    (add-nil-score-blobs (gmodule-tags
					       gmodule-object))))
  (define (add-nil-score-blobs gset-tags)
    "Return a list of nil-score-blobs for each tag in the
gset-tags list."
    (map add-nil-score-blob gset-tags))

  (define add-nil-blob
    (lambda (blobhash)
      "Return a newly created blob of gset-tag with score 0."
      (make-blob blobhash 'parents 'children 0 0)))

  ;; Return the scorecard-skeleton
  (make-scorecard (add-scorecard-data gmodule-object-list)))


(define (update-scorecard scorecard blobhash assessment-result)
  "Return a new scorecard, on the basis of SCORECARD, with the score
of the relevant blob adjusted based on ASSESSMENT-RESULT and
BLOBHASH."
  ((scorecard-data scorecard) 'put blobhash
   (update-blob ((scorecard-data scorecard) 'get blobhash)
		assessment-result)))

(define (update-blob blob assessment-result)
  "Return a new blob constructed on the basis of BLOB, with its score
adapted according to ASSESSMENT-RESULT and its counter progressed."
  (make-blob (blob-hash blob)
	     (blob-parents blob)
	     (blob-children blob)
	     (blob-score
	      (modify-score (blob-score blob)
			    assessment-result))
	     (blob-counter
	      (progress-counter (blob-counter blob)
				assessment-result))))

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
