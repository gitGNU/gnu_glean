;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft scorecard-ops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft gset-ops)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft gmodule-ops)
  #:export (make-empty-scorecard
	    null-scorecard?

	    car-gmod-blobs
	    cdr-gmod-blobs
	    null-gmod-blob?

	    car-gset-blobs
	    cdr-gset-blobs
	    null-gset-blobs?

	    make-dummy-gmod-blob
	    dummy-gmod-blob?

	    make-dummy-gset-blob
	    dummy-gset-blob?

	    update-scorecard

	    active-module?

	    lower-score?

	    make-scorecard-skeleton))

(define (make-empty-scorecard)
  "Return a scorecard with no scorecard-data."
  (make-scorecard '()))

(define (null-scorecard? scorecard)
  "Return #t if scorecard has no data."
  (null? (scorecard-data scorecard)))

(define (make-dummy-gset-blob)
  "Return a score-gset-blob with no real data."
  (make-gset-blob 'no-tag #f 0))

(define (make-dummy-gmod-blob)
  "Return a score-gmod-blob with only one dummy-score-gset-blob as its
data."
  (make-gmod-blob 'no-gmodule
		  (list (make-dummy-gset-blob))))

(define (dummy-gmod-blob? score-gmod-blob)
  "Return #t if score-gmod-blob is a dummy-gmod-blob."
  (if (and (eq? (gmod-blob-id score-gmod-blob) 'no-gmodule)
	   (dummy-gset-blob? (car (gmod-blob-data score-gmod-blob))))
      #t
      #f))

(define (dummy-gset-blob? score-gset-blob)
  "Return #t if score-gmod-blob is a dummy-gset-blob."
  (if (and (eq? (gset-blob-tag score-gset-blob) 'no-tag)
	   (not (gset-blob-score score-gset-blob))
	   (zero? (gset-blob-counter score-gset-blob)))
      #t
      #f))

;; Data abstraction for processing gset blobs.
(define (car-gmod-blobs scorecard-data)
  "Return the first score-gmod-blob in scorecard-data."
  (car scorecard-data))
(define (cdr-gmod-blobs scorecard-data)
  "Return the remaining score-gmod-blobs in scorecard-data."
  (cdr scorecard-data))
(define (null-gmod-blob? module-blob)
  "Return #t if scorecard-data contains no gmod-blobs."
  (null? (gmod-blob-data module-blob)))

;; Data abstraction for processing gset blobs.
(define (car-gset-blobs gset-blobs)
  "Return the first gset-blob within gset-blobs."
  (car gset-blobs))
(define (cdr-gset-blobs gset-blobs)
  "Return the rest of the gset-blobs in gset-blobs."
  (cdr gset-blobs))
(define (null-gset-blobs? gset-blobs)
  "Return #t if gset-blobs contains no gset-blobs."
  (null? gset-blobs))

(define (lower-score? gset-blob1 gset-blob2)
  "Return #t if gset-blob2 is dummy-gset-blob, or if its score is
lower than that of gset-blob1."
  (cond ((dummy-gset-blob? gset-blob2)
	 #t)
	((< (gset-blob-score gset-blob1)
	    (gset-blob-score gset-blob2))
	 #t)
	(else #f)))

(define (active-module? gmod-blob active-modules)
  "Return #t if the gmod-blob-id in gmod-blob is part of the
active-modules list."
  (member (gmod-blob-id gmod-blob)
	  active-modules))

(define (make-scorecard-skeleton gmodule-object-list)
  "Return a scorecard populated with a score-gmod-blob for each
gmodule-object, in turn containing a nil-score-gset-blob for each
gset-tag in that gmodule-objects' gmodule-parts section."

  (define (add-scorecard-data gmodule-object-list)
    "Return a list of gmod-blobs containing nil-score-gset-blobs for
each gmodule-object in the list."
    (map add-nil-score-gmod-blob gmodule-object-list))

  (define (add-nil-score-gmod-blob gmodule-object)
    "Return a gmod-blob containing a nil-score gset-blob for each
gset-tag in the gmodule object."
    (make-gmod-blob (gmodule-id gmodule-object)
		    (add-nil-score-gset-blobs (gmodule-tags
					       gmodule-object))))
  (define (add-nil-score-gset-blobs gset-tags)
    "Return a list of nil-score-gset-blobs for each tag in the
gset-tags list."
    (map add-nil-score-gset-blob gset-tags))

  (define add-nil-score-gset-blob
    (lambda (gset-tag)
      "Return a newly created gset-blob of gset-tag with score 0."
      (make-gset-blob gset-tag 0 0)))

  ;; Return the scorecard-skeleton
  (make-scorecard (add-scorecard-data gmodule-object-list)))


(define (update-scorecard scorecard gmodule-id gset-tag assessment-result)
  "Return a new scorecard, with the scores of each gset-blob adjusted
based on assessment-result."

  (define (gmod-blobs-worker gmod-blobs)
      "Return new GMOD-BLOBS list: recurse on blobs, replace GMODULE-ID
matching blob with updated one."

    (define (gset-blobs-worker gset-blobs)
      "Return new GSET-BLOBS list: recurse on blobs, replace GSET-TAG
matching blob with updated one."

      (define (gset-blob-inspector)
	"See if gset-blob is the blob we're looking for. If so return
a new, updated blob, otherwise simply return the blob."
	(lambda (gset-blob)
	  (if (eqv? (gset-blob-tag gset-blob) gset-tag)
	      (make-gset-blob
	       gset-tag
	       (modify-score (gset-blob-score gset-blob)
			     assessment-result)
	       (progress-counter (gset-blob-counter gset-blob)
				 assessment-result)))))

      (map (gset-blob-inspector) gset-blobs))

    (define (gmod-blob-inspector)
      "See if gmod-blob is the blob we're looking for. If so return
a new, updated blob, otherwise simply return the blob."
      (lambda (gmod-blob)
	(if (eqv? (gmod-blob-id gmod-blob) gmodule-id)
	    (make-gmod-blob 
	     gmodule-id
	     (gset-blobs-worker (gmod-blob-data gmod-blob)))
	    (gmod-blob))))

    (map (gmod-blob-inspector) gmod-blobs))

  ;; Make new scorecard with all old data except for the one updated
  ;; gset-blob.
  (make-scorecard (gmod-blobs-worker (scorecard-data scorecard))))

(define (modify-score old-score assessment-result)
  "Returns a score modified by an algorithm on the basis of
assessment-result, to take the place of old-score."
  (if assessment-result
      (+ old-score 1)
      old-score))

(define (progress-counter old-counter-value assessment-result)
  "Returns the increment of counter-value or 0 if counter-value
is equal to or greater the number of problems in the gset."
  (1+ old-counter-value))

