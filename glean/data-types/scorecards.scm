;;; glean --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (glean data-types scorecards)
  #:use-module (glean data-types sets)
  #:use-module (glean data-manager)
  #:use-module (glean utils)
  #:use-module (rnrs)
  #:use-module (srfi srfi-1)
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
            blob-properties
            blob-effects

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
				  (immutable counter)
                                  (immutable properties)
                                  (immutable effects))))
(define blob-rcd
  (make-record-constructor-descriptor blob-rtd #f #f))
(define make-blob (record-constructor blob-rcd))
(define blob? (record-predicate blob-rtd))
(define blob-hash (record-accessor blob-rtd 0))
(define blob-parents (record-accessor blob-rtd 1))
(define blob-children (record-accessor blob-rtd 2))
(define blob-score (record-accessor blob-rtd 3))
(define blob-counter (record-accessor blob-rtd 4))
(define blob-properties (record-accessor blob-rtd 5))
(define blob-effects (record-accessor blob-rtd 6))

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
  (make-blob 'no-tag '() '() #f 0 '() '()))

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
  "Return #t if blob1's score is lower than that of blob2, or if blob1 has the
'tutorial property or effect. Return #f if blob1 is dummy-blob, or if its
larger than blob2."
  ;; FIXME: It would make more sense for this procedure to return the higher
  ;; priority blob (lower score, or active tutorial), rather than return #t or
  ;; #f.  Or rather, it would make more sense for the procedure that calls
  ;; this to call a procedure that works as described above.
  (let ((log #f)
        (res (cond ((not (and (blob? blob2) (blob? blob1)))
                    (error "lower-score? -- invalid blob1/2." blob1 blob2))
                   ((tutorial-in-progress? blob2)
                    (cons  "blob2 in progress. -> defer." #f))
                   ((incomplete-tutorial? blob1)
                    (cons "blob1 incomplete! -> blob1." #t))
                   ((complete-tutorial? blob1)
                    (cons "blob1 is complete! -> defer."#f))
                   ((dummy-blob? blob1)
                    (cons "blob1 is a dummy. -> defer." #f))
                   ((> (blob-score blob1) (blob-score blob2))
                    (cons "blob1 is bigger -> defer." #f))
                   (else
                    (cons "blob1 is smaller! -> blob1." #t)))))
    (if log (format #t "~a\n" (car res)))
    (cdr res)))

(define (property-or-effect? key blob)
  "Return #t if property or effect identified by KEY is active in BLOB, #f
otherwise."
  (let ((prop (assoc key (blob-properties blob)))
        (effect (assoc key (blob-effects blob))))
    (or (and (pair? prop) (cdr prop))
        (and (pair? effect) (cdr effect)))))

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
of the blob identified by BLOBHASH, and its parents, adjusted based on
ASSESSMENT-RESULT."
  (let* ((data (scorecard-data scorecard))
         (initial-blob (data 'get blobhash)))

    (define (update-skorecard data blobhash)
      (let ((blob (data 'get blobhash)))
        (define (update-data)
          (let ((new-blob (update-blob blob assessment-result initial-blob
                                       (number-of-child-blobs blob scorecard))))
            (data 'put blobhash new-blob)
            data))
        (if (null? (blob-parents blob))
            (update-data)
            ;; INFO: Currently this only modifies the car of parents,
            ;; which could conceivably contain more than one element. We
            ;; may need to process the other parents too… perhaps.
            (update-skorecard (update-data) (car (blob-parents blob))))))

    (make-scorecard (update-skorecard data blobhash))))

(define number-of-child-blobs
  (let ((previous (make-hash-table)))
    (lambda (blob scorecard)
      "Return the total number of children associated with BLOB in SCORECARD.
The procedure is memoized as it will be called often and repeatedly for the
same inputs."
      (let ((data (scorecard-data scorecard)))
        (define (child-hashes->child-blobs hashes)
          (map (lambda (blobhash) (data 'get blobhash)) hashes))
        (define (num-of-child-blobs-helper child-hashes total)
          (or (hash-ref previous child-hashes)
              (if (null? child-hashes) total
                  (hash-set! previous
                             child-hashes
                             (fold num-of-child-blobs-helper
                                   (+ total (length child-hashes))
                                   (map blob-children
                                        (child-hashes->child-blobs
                                         child-hashes)))))))

        (if (and (blob? blob) (procedure? data))
            (num-of-child-blobs-helper (blob-children blob) 0)
            (error 'number-of-child-blobs "Blob or data not right."))))))

(define (update-blob blob assessment-result initial-blob
                     number-of-child-blobs)
  "Return a new blob constructed on the basis of BLOB, with its score
adapted according to ASSESSMENT-RESULT, its counter progressed, and its
effects updated if INITIAL-BLOB contains 'tutorial key.."
  (make-blob (blob-hash blob)
	     (blob-parents blob)
	     (blob-children blob)
             (modify-score (blob-score blob)
                           assessment-result
                           number-of-child-blobs)
             (progress-counter (blob-counter blob)
                               assessment-result)
             (blob-properties blob)
             (consider-effects/properties blob assessment-result initial-blob
                                          number-of-child-blobs)))

(define (consider-effects/properties blob assessment-result initial-blob
                                     number-of-child-blobs)
  "Specialized dispatch to create blob-effects on the basis of the blob's
current context, and properties. Returns a blob-effects suitable assoc-list."
  (cond ((and (property-or-effect? 'tutorial initial-blob)
              (blob-score-completed-tutorial? (modify-score (blob-score blob)
                                                            assessment-result
                                                            number-of-child-blobs)))
         (remove-effect 'tutorial (blob-effects blob)))
        ((and (property-or-effect? 'tutorial initial-blob)
              (incomplete-tutorial? initial-blob))
         (add-effect 'tutorial (blob-effects blob)))
        (else (blob-effects blob))))

(define (add-effect key blob-effects)
  "Return a new blob-effects if BLOB-EFFECTS does not contain KEY. Return
BLOB-EFFECTS otherwise."
  (let ((current (assoc key blob-effects)))
    (if current blob-effects (acons key #t blob-effects))))

(define (remove-effect key blob-effects)
  "Return a new blob-effects with KEY removed, if BLOB-EFFECTS contains
KEY. Return BLOB-EFFECTS otherwise."
  (let ((current (assoc key blob-effects)))
    (if current
        (filter (lambda (acons-pair)
                  (not (eqv? key (car acons-pair))))
                blob-effects)
        blob-effects)))

(define (modify-score old-score assessment-result number-of-child-blobs)
  "Returns a score modified by an algorithm on the basis of
assessment-result, to take the place of old-score."
  (if assessment-result
      (+ old-score (if (< 0 number-of-child-blobs)
                       (/ 1 number-of-child-blobs)
                       1))
      old-score))

(define (progress-counter old-counter-value assessment-result)
  "Returns the increment of counter-value or 0 if counter-value
is equal to or greater the number of problems in the gset."
  (1+ old-counter-value))

;;;;; Tutorial Property Considerations
(define (blob-score-completed-tutorial? score) (>= score 1))

(define (complete-tutorial? blob)
  "Return #t if BLOB is a tutorial and it is \"complete\", i.e. it and all
children have been solved at least once. Return #f if it is not yet complete."
  (and (property-or-effect? 'tutorial blob)
       (blob-score-completed-tutorial? (blob-score blob))))

(define (incomplete-tutorial? blob)
  "Return #t if BLOB is a tutorial and it is \"incomplete\", i.e. it or one
of its children have not yet been solved. Return #f if it is complete."
  (and (property-or-effect? 'tutorial blob)
       (not (blob-score-completed-tutorial? (blob-score blob)))))

(define (tutorial-in-progress? blob)
  "Return #t if BLOB is a tutorial and it is currently in progress, #f
otherwise."
  (and (property-or-effect? 'tutorial blob) ; is tutorial
       (> (blob-score blob) 0)              ; tutorial started
       (incomplete-tutorial? blob)))        ; tutorial not complete.
