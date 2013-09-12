;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types gprofiles)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft utils)
  #:export (make-profile
	    profile?
	    get-id
	    get-name
	    get-active-modules
	    get-scorecard
	    
	    make-id
	    id?))

(define-record-type <id>
  (make-id n t)
  id?
  (n get-id-n)
  (t get-id-t))

(define-record-type* <profile>
  make-profile make-gprofile
  profile?
  (name get-name)		      ; Human friendly name
  (id get-id)			      ; Unique profile identifier;
  (active-modules get-active-modules) ; List of currently active gmodules
  (scorecard get-scorecard))	      ; List of gset counters for
					; currently used gsets
