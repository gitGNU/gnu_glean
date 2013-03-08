;;; guilecraft --- user profile data-types.         -*- coding: utf-8 -*-

(define-module (guilecraft gprofiles)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft scorecards)
  #:export (gprof_make-profile
	    gprof_profile?
	    gprof_get-UUID
	    gprof_get-name
	    gprof_get-active-modules
	    gprof_get-scorecard

	    gprof_first-active-module
	    gprof_rest-active-modules
	    gprof_empty-active-modules?
	    gprof_active-module?))

(define-record-type <profile>
  (gprof_make-profile UUID name active-modules scorecard)
  gprof_profile?
  (UUID gprof_get-UUID)	       ; Unique profile identifier; timestamp?
  (name gprof_get-name)	       ; Human friendly name
  (active-modules gprof_get-active-modules) ; List of currently active gmodules
  (scorecard gprof_get-scorecard))	      ; List of gset counters for
					; currently used gsets
(define gprof_first-active-module
  (lambda (active-modules)
    (car active-modules)))

(define gprof_rest-active-modules
  (lambda (active-modules)
    (cdr active-modules)))

(define gprof_empty-active-modules?
  (lambda (active-modules)
    (null? active-modules)))

(define gprof_active-module?
  (lambda (scorecard-datum active-modules)
    (member (scc_get-scorecard-datum-gmodule-name scorecard-datum) 
	    active-modules)))

;;; Player profile scores are stored as an association list embedded
;;; within an association list, e.g:
;;; (
;;; (git-gmodule . (git-branch . 0)
;;;                (git-log . 5))
;;; (scheme-gmodule . (scheme-lists . 3)
;;;                   (scheme-lambda .1))
;;;                   )
;;; Would return git-branch as the next challenge.
