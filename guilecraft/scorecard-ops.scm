;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft scorecard-ops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft gset-ops)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft gmodule-ops)
  #:export (make-empty-scorecard
	    null-scorecard?

	    car-mod-blobs
	    cdr-mod-blobs
	    null-mod-blob?

	    car-set-blobs
	    cdr-set-blobs
	    null-set-blobs?

	    make-dummy-mod-blob
	    dummy-mod-blob?

	    make-dummy-set-blob
	    dummy-set-blob?

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

(define (make-dummy-set-blob)
  "Return a score-set-blob with no real data."
  (make-set-blob 'no-tag #f 0))

(define (make-dummy-mod-blob)
  "Return a score-mod-blob with only one dummy-score-set-blob as its
data."
  (make-mod-blob 'no-gmodule
		  (list (make-dummy-set-blob))))

(define (dummy-mod-blob? score-mod-blob)
  "Return #t if score-mod-blob is a dummy-mod-blob."
  (if (and (eq? (mod-blob-id score-mod-blob) 'no-gmodule)
	   (dummy-set-blob? (car (mod-blob-data score-mod-blob))))
      #t
      #f))

(define (dummy-set-blob? score-set-blob)
  "Return #t if score-mod-blob is a dummy-set-blob."
  (if (and (eq? (set-blob-id score-set-blob) 'no-tag)
	   (not (set-blob-score score-set-blob))
	   (zero? (set-blob-counter score-set-blob)))
      #t
      #f))

;; Data abstraction for processing gset blobs.
(define (car-mod-blobs scorecard-data)
  "Return the first score-mod-blob in scorecard-data."
  (car scorecard-data))
(define (cdr-mod-blobs scorecard-data)
  "Return the remaining score-mod-blobs in scorecard-data."
  (cdr scorecard-data))
(define (null-mod-blob? module-blob)
  "Return #t if scorecard-data contains no mod-blobs."
  (null? (mod-blob-data module-blob)))

;; Data abstraction for processing gset blobs.
(define (car-set-blobs set-blobs)
  "Return the first set-blob within set-blobs."
  (car set-blobs))
(define (cdr-set-blobs set-blobs)
  "Return the rest of the set-blobs in set-blobs."
  (cdr set-blobs))
(define (null-set-blobs? set-blobs)
  "Return #t if set-blobs contains no set-blobs."
  (null? set-blobs))

(define (lower-score? set-blob1 set-blob2)
  "Return #t if set-blob2 is dummy-set-blob, or if its score is
lower than that of set-blob1."
  (cond ((dummy-set-blob? set-blob2)
	 #t)
	((< (set-blob-score set-blob1)
	    (set-blob-score set-blob2))
	 #t)
	(else #f)))

(define (active-module? mod-blob active-modules)
  "Return #t if the mod-blob-id in mod-blob is part of the
active-modules list."
  (member (mod-blob-id mod-blob)
	  active-modules))

(define (make-scorecard-skeleton gmodule-object-list)
  "Return a scorecard populated with a score-mod-blob for each
gmodule-object, in turn containing a nil-score-set-blob for each
gset-tag in that gmodule-objects' gmodule-parts section."

  (define (add-scorecard-data gmodule-object-list)
    "Return a list of mod-blobs containing nil-score-set-blobs for
each gmodule-object in the list."
    (map add-nil-score-mod-blob gmodule-object-list))

  (define (add-nil-score-mod-blob gmodule-object)
    "Return a mod-blob containing a nil-score set-blob for each
gset-tag in the gmodule object."
    (make-mod-blob (set-id gmodule-object)
		    (add-nil-score-set-blobs (gmodule-tags
					       gmodule-object))))
  (define (add-nil-score-set-blobs gset-tags)
    "Return a list of nil-score-set-blobs for each tag in the
gset-tags list."
    (map add-nil-score-set-blob gset-tags))

  (define add-nil-score-set-blob
    (lambda (gset-tag)
      "Return a newly created set-blob of gset-tag with score 0."
      (make-set-blob gset-tag 0 0)))

  ;; Return the scorecard-skeleton
  (make-scorecard (add-scorecard-data gmodule-object-list)))

(define (update-scorecard scorecard gmodule-id gset-tag assessment-result)
  "Return a new scorecard, with the scores of each set-blob adjusted
based on assessment-result."

  (define (mod-blobs-worker mod-blobs)
      "Return new MOD-BLOBS list: recurse on blobs, replace GMODULE-ID
matching blob with updated one."

    (define (set-blobs-worker set-blobs)
      "Return new SET-BLOBS list: recurse on blobs, replace GSET-TAG
matching blob with updated one."

      (define (set-blob-inspector)
	"See if set-blob is the blob we're looking for. If so return
a new, updated blob, otherwise simply return the blob."
	(lambda (set-blob)
	  (if (eqv? (set-blob-id set-blob) gset-tag)
	      (make-set-blob
	       gset-tag
	       (modify-score (set-blob-score set-blob)
			     assessment-result)
	       (progress-counter (set-blob-counter set-blob)
				 assessment-result)))))

      (map (set-blob-inspector) set-blobs))

    (define (mod-blob-inspector)
      "See if mod-blob is the blob we're looking for. If so return
a new, updated blob, otherwise simply return the blob."
      (lambda (mod-blob)
	(if (eqv? (mod-blob-id mod-blob) gmodule-id)
	    (make-mod-blob 
	     gmodule-id
	     (set-blobs-worker (mod-blob-data mod-blob)))
	    mod-blob)))

    (map (mod-blob-inspector) mod-blobs))

  ;; Make new scorecard with all old data except for the one updated
  ;; set-blob.
  (make-scorecard (mod-blobs-worker (scorecard-data scorecard))))

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
