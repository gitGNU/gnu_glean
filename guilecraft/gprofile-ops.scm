;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft gprofile-ops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types gprofiles)
  #:export (first-active-module
	    rest-active-modules
	    empty-active-modules?
	    update-id
	    trad-make-profile))

(define first-active-module
  (lambda (active-modules)
    (car active-modules)))

(define rest-active-modules
  (lambda (active-modules)
    (cdr active-modules)))

(define empty-active-modules?
  (lambda (active-modules)
    (null? active-modules)))

(define create-profile-id
  (lambda (name)
    "Generate the ID on the basis of name and current-time."
    (make-id name (current-time))))

(define update-id
  (lambda (profile)
    "Convenience procedure to generate new ID for existing profile."
    (create-profile-id (profile-name profile))))

(define (trad-make-profile n i a-m sc)
  (make-profile
   n
   i
   a-m
   sc))
