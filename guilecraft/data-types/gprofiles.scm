;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types gprofiles)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft utils)
  #:export (gprof_make-profile
	    gprof_profile?
	    gprof_get-id
	    gprof_get-name
	    gprof_get-active-modules
	    gprof_get-scorecard
	    
	    gprof_make-id
	    gprof_id?))

(define-record-type <id>
  (gprof_make-id n t)
  gprof_id?
  (n get-id-n)
  (t get-id-t))

(define-record-type* <profile>
  gprof_make-profile make-profile
  gprof_profile?
  (name gprof_get-name)	       ; Human friendly name
  (id gprof_get-id)	       ; Unique profile identifier;
  (active-modules gprof_get-active-modules) ; List of currently active gmodules
  (scorecard gprof_get-scorecard))	      ; List of gset counters for
					; currently used gsets
