;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft gprofile-ops)
  #:use-module (rnrs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types gprofiles)
  #:export (first-active-module
	    rest-active-modules
	    empty-active-modules?
	    update-id
	    trad-make-profile
	    name->hash
	    id->hash))

(define first-active-module
  (lambda (active-modules)
    (car active-modules)))

(define rest-active-modules
  (lambda (active-modules)
    (cdr active-modules)))

(define empty-active-modules?
  (lambda (active-modules)
    (null? active-modules)))

(define (create-profile-id name)
  "Generate the ID on the basis of name and current-time."
  (make-id (string->symbol name) (string->symbol
				  (number->string
				   (current-time)))))

(define (update-id profile)
  "Convenience procedure to generate new ID for existing profile."
  (create-profile-id (profile-name profile)))

(define (id->hash profile-id)
  (if (id? profile-id)
      (symbol-hash (symbol-append (id-symbol profile-id)
				  (id-stamp profile-id)))
      (assertion-violation
       'id->hash
       "PROFILE-ID is not a profile id."
       profile-id)))

(define (id->hash profile-name)
  (if (string? profile-name)
      (string-hash profile-name)
      (assertion-violation
       'name->hash
       "PROFILE-NAME is not a profile string."
       profile-name)))

(define (trad-make-profile n i a-m sc)
  (make-profile
   n
   i
   a-m
   sc))
