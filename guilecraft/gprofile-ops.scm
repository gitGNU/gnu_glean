;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft gprofile-ops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types gprofiles)
  #:export (gprof_first-active-module
	    gprof_rest-active-modules
	    gprof_empty-active-modules?
	    gprof_update-id))

(define gprof_first-active-module
  (lambda (active-modules)
    (car active-modules)))

(define gprof_rest-active-modules
  (lambda (active-modules)
    (cdr active-modules)))

(define gprof_empty-active-modules?
  (lambda (active-modules)
    (null? active-modules)))

(define create-profile-id
  (lambda (name)
    "Generate the ID on the basis of name and current-time."
    (gprof_make-id name (current-time))))

(define gprof_update-id
  (lambda (profile)
    "Convenience procedure to generate new ID for existing profile."
    (create-profile-id (gprof_get-name profile))))
