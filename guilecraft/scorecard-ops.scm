;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft scorecard-ops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft gmodule-ops)
  #:use-module (guilecraft gmodule-manager)
  #:export (make-empty-scorecard
	    null-scorecard?

	    car-gmod-blobs
	    cdr-gmod-blobs
	    null-gmod-blobs?

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
  (null-gmod-blobs? (scorecard-data scorecard)))

(define (make-dummy-gset-blob)
  "Return a score-gset-blob with no real data."
  (make-gset-blob 'no-tag #f))

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
	   (not (gset-blob-score score-gset-blob)))
      #t
      #f))

;; Data abstraction for processing gset blobs.
(define (car-gmod-blobs scorecard-data)
  "Return the first score-gmod-blob in scorecard-data."
  (car scorecard-data))
(define (cdr-gmod-blobs scorecard-data)
  "Return the remaining score-gmod-blobs in scorecard-data."
  (cdr scorecard-data))
(define (null-gmod-blobs? scorecard-data)
  "Return #t if scorecard-data contains no gmod-blobs."
  (null? scorecard-data))

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
      (make-gset-blob gset-tag 0)))

  ;; Return the scorecard-skeleton
  (make-scorecard (add-scorecard-data gmodule-object-list)))


(define (update-scorecard scorecard gmodule-id gset-tag assessment-result)
  "Return a new scorecard, with the scores of each gset-blob adjusted
based on assessment-result."

  (define (modify-score old-score assessment-result)
    "Returns a score modified by an algorithm on the basis of
assessment-result, to take the place of old-score."
    (if assessment-result
	(+ old-score 1)
	old-score))

  (define (gset-blobs-worker old-gset-blobs gset-tag assessment-result
			     new-gset-blobs)
    "Returns a list of gset-blobs with the gset-blob whose tag matches
gset-tag having its score modified by modify-score."
    (cond ((null? old-gset-blobs)
	   (reverse new-gset-blobs))
	  ((eq? (gset-blob-tag (car-gset-blobs old-gset-blobs))
		gset-tag)
	   (gset-blobs-worker (cdr-gset-blobs old-gset-blobs)
			      gset-tag
			      assessment-result
			      (cons (make-gset-blob
				     gset-tag
				     (modify-score (gset-blob-score
						    (car-gset-blobs old-gset-blobs))
						   assessment-result))
				    new-gset-blobs)))
	  (else (gset-blobs-worker (cdr old-gset-blobs)
				   gset-tag
				   assessment-result
				   (cons (car old-gset-blobs)
					 new-gset-blobs)))))

  (define (gmod-blobs-worker old-gmod-blobs gmodule-id gset-tag
			     assessment-result new-gmod-blobs)
    "Returns a list of gmod-blobs with the gmod-blob whose id matches
gmodule-id having its gset-blobs modified by gset-blobs-worker."
    (cond ((null? old-gmod-blobs)
	   (reverse new-gmod-blobs))
	  ((eq? (gmod-blob-id (car old-gmod-blobs))
		gmodule-id)
	   (gmod-blobs-worker (cdr old-gmod-blobs)
			      gmodule-id
			      gset-tag
			      assessment-result
			      (cons
			       (make-gmod-blob
				gmodule-id
				(gset-blobs-worker
				 (gmod-blob-data (car old-gmod-blobs))
				 gset-tag
				 assessment-result
				 '()))
			       new-gmod-blobs)))
	  (else (gmod-blobs-worker (cdr old-gmod-blobs)
				   gmodule-id
				   gset-tag
				   assessment-result
				   (cons (car old-gmod-blobs)
					 new-gmod-blobs)))))

  ;; Return the updated scorecard
  (make-scorecard (gmod-blobs-worker (scorecard-data scorecard)
				     gmodule-id
				     gset-tag
				     assessment-result
				     '())))
