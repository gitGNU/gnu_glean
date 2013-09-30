;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft profiler)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types gprofiles)	; For profile data-struct
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft scorecard-ops)

  #:export (profiler
	    lowest-scoring-gset))

;;; Commentary:
;;
;; Profiler provides an algorithm to retrieve the next eligible
;; candidate problem by assessing the player's profile.
;;
;; Currently it simply returns the lowest scoring gset-tag or
;; gmodule-id of the gmodule containing the lowest scoring gset.
;;
;;; Code:

(define profiler
  (lambda (profile)
    "Returns lowest scoring gmod-blob."
    (lowest-module-blob profile)))

(define (lowest-module-blob profile)
  "Returns the lowest scoring module blob from PROFILE, taking into
account its list of active modules.

Returns false if no active modules exist in profile.

Returns an error if there is no data PROFILE's scorecard."

  (let ((active-modules (get-active-modules profile))
	(scorecard (get-scorecard profile)))

    (cond ((empty-active-modules? active-modules)
	   #f)
	  ((null-scorecard? scorecard)
	   (error "profiler: No data in scorecard!"))
	  (else (reduce (lambda (module-blob previous)
			  (if (and (active-module? module-blob
						   active-modules)
				   (lower-score?
				    (lowest-scoring-gset module-blob)
				    (lowest-scoring-gset previous)))
			      module-blob
			      previous))
			'bogus-option
			(scorecard-data scorecard))))))

(define (lowest-scoring-gset gmod-blob)
  "Return the lowest scoring gset-blob in the given
gmod-blob.

Raise an error if GMOD-BLOB contains no gset-blobs."
  (if (null-gmod-blob? gmod-blob)
      (error "lowest-scoring-gset: No data in gset!")
      (reduce (lambda (gset-blob previous)
		(if (lower-score? gset-blob
				  previous)
		    gset-blob
		    previous))
	      'bogus-option
	      (gmod-blob-data gmod-blob))))
