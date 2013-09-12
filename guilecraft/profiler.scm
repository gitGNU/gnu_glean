;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft profiler)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types gprofiles)	; For profile data-struct
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft scorecard-ops)

  #:export (prof_profiler))

;;; Commentary:
;;
;; Profiler provides an algorithm to retrieve the next eligible
;; candidate problem by assessing the player's profile.
;;
;; Currently it simply returns the lowest scoring gset-tag or
;; gmodule-id of the gmodule containing the lowest scoring gset.
;;
;;; Code:

(define prof_profiler
  (lambda (message profile)
    "Returns profile challenge data by evaluating the scores stored in
the profile."
    (cond ((eq? message 'get-gset-tag)
	   ;; Return the lowest scoring gset-tag from all active modules
	   (return-lowest-scoring-tag profile))
	  ((eq? message 'get-gset-gmodule)
	   ;; Return gmodule name of lowest scoring gset-tag
	   (return-lowest-scoring-gmodule profile))
	  (else (error "prof_profiler: unknown message: " message)))))

(define return-lowest-scoring-tag
  (lambda (profile)
    (define (gset-blob-tag-from-gmod-blob score-gmod-blob)
      (gset-blob-tag (gmod-blob-data score-gmod-blob)))
    "Convenience function to return the lowest scoring tag from a
given profile"
    (return-lowest-scoring-tag-or-gmodule gset-blob-tag-from-gmod-blob profile)))

(define return-lowest-scoring-gmodule
  (lambda (profile)
    "Convenience function to return the lowest scoring gmodule from a
given profile"
    (return-lowest-scoring-tag-or-gmodule gmod-blob-id profile)))

;; Function below carries out meat of computation. It relies on
;; properly exposed data structures defined in the gprofiles module,
;; to access the data stored within it.

(define (return-lowest-scoring-tag-or-gmodule proc profile)
  "Returns/extracts the lowest scoring tag or gmodule from the given
profile."

  ;; return scorecard data for active gmodules
  (define (iter active-modules scorecard lowest-scoring-gmod-blob)
    (let ([scorecard-data (scorecard-data scorecard)])
      (cond ((empty-active-modules? active-modules)
	     (error "return-lowest-scoring-tag-or-gmodule: Profile
contains no active modules: " profile))

	    ;; reached end of profile scorecard, call
	    ;; gset-blobs-worker to scan tmp- lowest scoring blob
	    ((null-gmod-blobs? scorecard-data)
	     lowest-scoring-gmod-blob)

	    ;; if first gmodule blob is an active module, test whether
	    ;; its lowest scoring gset blob is lower scoring than the
	    ;; current lowest scoring gset blob.
	    ((active-module? (car-gmod-blobs scorecard-data)
			     active-modules)
	     (let ([lowest-scoring-in-gmod-blob
		    (lowest-in-gmod-blob (car-gmod-blobs scorecard-data))])
	       (cond ((lower-score? lowest-scoring-in-gmod-blob
				    (car (gmod-blob-data lowest-scoring-gmod-blob)))
		      ;; If so, save current gmod blob id and current
		      ;; gset blob id, and recurse onwards
		      (iter active-modules
			    (make-scorecard (cdr-gmod-blobs scorecard-data))
			    (make-gmod-blob
			     (gmod-blob-id
			      (car-gmod-blobs scorecard-data))
			     lowest-scoring-in-gmod-blob)))
		     ;; Else, recurse onwards.
		     (else (iter active-modules
				 (make-scorecard (cdr-gmod-blobs scorecard-data))
				 lowest-scoring-gmod-blob)))))
	    (else (iter active-modules
			(make-scorecard (cdr-gmod-blobs scorecard-data))
			lowest-scoring-gmod-blob)))))

  ;; Call iter with populated list, and return blob applied to proc.
  ;; Proc should retrieve data field in blob, else will cause
  ;; problems.
  (proc (iter (get-active-modules profile)
	      (get-scorecard profile)
	      (make-dummy-gmod-blob))))

(define (lowest-in-gmod-blob gmod-blob)
  "Return the lowest scoring score-gset-blob in the given
gmod-blob."
  (define (iter tmp-gset-blobs lowest-scoring-gset-blob)
    (cond ((null? tmp-gset-blobs)
	   lowest-scoring-gset-blob)
	  ((lower-score? (car-gset-blobs tmp-gset-blobs)
			 lowest-scoring-gset-blob)
	   (iter (cdr-gset-blobs tmp-gset-blobs)
		 (car-gset-blobs tmp-gset-blobs)))
	  (else (iter (cdr-gset-blobs tmp-gset-blobs)
		      lowest-scoring-gset-blob))))

  (iter (gmod-blob-data gmod-blob)
	(make-dummy-gset-blob)))
