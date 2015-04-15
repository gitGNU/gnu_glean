;; scorecards.scm --- scorecard data type definition   -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
;;
;; This file is part of Glean.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Scorecards contain the results of player challenge responses, and are thus
;; the key data type in the lounge.
;;
;; A scorecard contains many blobs, which are the record-type that actually
;; tracks progress. Blobs also contain within them the abstract representation
;; of disciplines, through the use of parent and children fields.
;;
;; This currently means that that abstract representation is duplicated for
;; each profile stored in the lounge.  It also feels like a serious blurring
;; of concerns.  I would like to split the abstract representation into a
;; separate lounge module, and merely use score-cards to track individual
;; results (referring to the abstract representation module as and when
;; necessary).
;;
;; They are currently implemented as a hashtable contained in a 1 field record
;; — both should change.
;;
;;; Code:

(define-module (glean lounge scorecards)
  #:use-module (glean common utils)
  #:use-module (ice-9 match)
  #:use-module (rnrs hashtables)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (
            <scorecard>
            make-scorecard
            scorecard?
            scorecard-data
            
            <blob>
            make-blob
            blob?
            blob-hash
            blob-parents
            blob-children
            blob-score
            blob-counter
            blob-properties
            blob-effects
            blob-base-lexp
            blob-dag-hash
            
            make-empty-scorecard
            empty-scorecard?
            
            make-dummy-blob
            dummy-blob?
            find-blob
            add-blobs
            
            update-scorecard
            
            lower-score?
            blobhash?
            ))


;;;;; Record Type Definitions

(define-record-type <blob>
  (make-blob hash parents children score counter properties effects base-lexp
             dag-hash)
  blob?
  (hash blob-hash)
  (parents blob-parents)
  (children blob-children)
  (score blob-score)
  (counter blob-counter)
  (properties blob-properties)
  (effects blob-effects)
  (base-lexp blob-base-lexp)
  (dag-hash blob-dag-hash))

(define-record-type <scorecard>
  (make-scorecard data)
  scorecard?
  (data scorecard-data))


;;;;; Scorecard Operations

(define (find-blob blobhash scorecard)
  "Return the blob stored under BLOBHASH in SCORECARD."
  (let ((data (scorecard-data scorecard)))
    (define (list-blobs)
      (data 'values))
    ((scorecard-data scorecard) 'get blobhash)))

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


;;;;; Blob Operations

(define (blobhash? obj)
  "Return #t if OBJ is a blobhash, #f otherwise."
  (string? obj))

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
                                          number-of-child-blobs)
             (blob-base-lexp blob)
             (blob-dag-hash blob)))

(define (make-dummy-blob)
  "Return a score-blob with no real data."
  (make-blob 'no-tag '() '() #f 0 '() '() '() ""))

(define (dummy-blob? score-blob)
  "Return #t if score-mod-blob is a dummy-blob, #f otherwise."
  (match score-blob
    (($ <blob> 'no-tag par c #f 0 p e b d) #t)
    (_ #f)))

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


;;;;; Properties and/or Effects

(define (property-or-effect? key blob)
  "Return #t if property or effect identified by KEY is active in BLOB, #f
otherwise."
  (let ((prop (assoc key (blob-properties blob)))
        (effect (assoc key (blob-effects blob))))
    (or (and (pair? prop) (cdr prop))
        (and (pair? effect) (cdr effect)))))

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


;;;; Tutorial Property Considerations
(define (blob-score-completed-tutorial? score)
  "Return #t if SCORE is considered to reflect a completed tutorial.  At
present this is the case if SCORE is greater than or equal 1."
  (>= score 1))

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


;;;;; Data Manager
;; FIXME: THIS IS NON-FUNCTIONAL!  Only used by scorecards.

(define data-manager
  (lambda (predicate)
    "An old-school, non-functional (hash-table based) non-persistent
data-store to store scorecard data.  This is crufty, and should be abandoned
ASAP!"
    (let ([data (make-hashtable string-hash string=?)])
      (lambda (message . args)
        "Return the value associated with (car ARGS) if MESSAGE is
'get, or #f if the (car ARGS) is not a key in the hashtable.

 Return #t if MESSAGE is 'put, and (cadr ARGS) passes the predicate
check and is added to table. Return #f otherwise.

Return all values stored in the table when MESSAGE is 'list."
        (cond ((eqv? message 'get)      ; Retrieving an entry
               (hashtable-ref data (car args) #f))
              ((eqv? message 'put)        ; Storing an entry
               (if (predicate (cadr args))
                   (begin
                     (hashtable-set! data (car args) (cadr args))
                     #t)
                   #f))
              ((eqv? message 'rem-val)
               (call-with-values
                   ;; fetch values and keys
                   (lambda () (hashtable-entries data))
                 (lambda (ks vs)
                   (vector-for-each
                    ;; destroy every entry who's value matches arg to
                    ;; 'rem-val
                    (lambda (k v)
                      (if (equal? v (car args))
                          (hashtable-delete! data k)))
                    ks vs))))
              ((eqv? message 'rem)
               (hashtable-delete! data (car args)))
              ((eqv? message 'list-keys (hashtable-keys data)))
              ((eqv? message 'list) (hashtable-entries data))
              ((eqv? message 'values)
               (vector->list
                (vector-map (lambda (key)
                              (hashtable-ref data key #f))
                            (hashtable-keys data))))
              ((eqv? message 'contains)
               (hashtable-contains? data (car args)))
              ((eqv? message 'update)
               (hashtable-update! data (car args)
                                  (cadr args) (caddr args)))
              (else (error "data-manager: unknown message: " message)))))))

;;; scorecards.scm ends here
