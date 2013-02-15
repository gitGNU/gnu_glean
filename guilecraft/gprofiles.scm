;;; guilecraft --- user profile data-types.         -*- coding: utf-8 -*-

(define-module (guilecraft gprofiles)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-gprofile
	    gprofile?
	    get-UUID
	    get-name
	    get-active-gmodules
	    get-gset-counters))

(define-record-type <gprofile>
  (make-gprofile UUID name active-gmodules gset-counters)
  gprofile?
  (UUID get-UUID)                          ; Unique profile identifier; timestamp?
  (name get-name)                          ; Human friendly name
  (active-gmodules get-active-gmodules)    ; List of currently active gmodules
  (gset-counters get-gset-counters))       ; List of gset counters for currently used gsets
