;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft profiler)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs)
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

(define (profiler profile)
  "Returns lowest scoring mod-blob."
  (if (and (profile? profile)
	   (not (null-scorecard? (profile-scorecard profile))))
      (lowest-module-blob profile)
      (assertion-violation
       'profiler
       "PROFILE is not a profile, or contains a null-scorecard."
       profile (scorecard-data profile))))

(define (lowest-module-blob profile)
  "Returns the lowest scoring module blob from PROFILE, taking into
account its list of active modules.

Returns false if no active modules exist in profile, or if the
scorecard is empty."

  (let ((active-modules (profile-active-modules profile))
	(scorecard (profile-scorecard profile)))

    (cond ((or (empty-active-modules? active-modules)
	       (null-scorecard? scorecard))
	   #f)
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

(define (lowest-scoring-gset mod-blob)
  "Return the lowest scoring set-blob in the given
mod-blob.

Raise an error if MOD-BLOB contains no set-blobs."
  (if (null-mod-blob? mod-blob)
      (assertion-violation
       'lowest-scoring-gset
       "MOD-BLOB contains no data!"
       mod-blob)
      (reduce (lambda (set-blob previous)
		(if (lower-score? set-blob
				  previous)
		    set-blob
		    previous))
	      'bogus-option
	      (mod-blob-data mod-blob))))
