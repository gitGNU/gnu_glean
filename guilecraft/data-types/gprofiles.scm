;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft utils)
  #:use-module (rnrs records procedural)
  #:export (make-profile
	    profile?
	    profile-name
	    profile-id
	    profile-active-modules
	    profile-scorecard

	    make-id
	    id?))

(define profile-rtd
  (make-record-type-descriptor 'profile #f #f #f #f
			       '#((immutable name)
				  (immutable id)
				  (immutable active-modules)
				  (immutable scorecard))))
(define profile-rcd
  (make-record-constructor-descriptor profile-rtd #f #f))
(define make-profile (record-constructor profile-rcd))
(define profile? (record-predicate profile-rtd))
(define profile-name (record-accessor profile-rtd 0))
(define profile-id (record-accessor profile-rtd 1))
(define profile-active-modules (record-accessor profile-rtd 2))
(define profile-scorecard (record-accessor profile-rtd 3))

(define id-rtd
  (make-record-type-descriptor 'id #f #f #f #f
			       '#((immutable symbol)
				  (immutable stamp))))
(define id-rcd
  (make-record-constructor-descriptor id-rtd #f #f))
(define make-id (record-constructor id-rcd))
(define id? (record-predicate id-rtd))
(define id-symbol (record-accessor id-rtd 0))
(define id-stamp (record-accessor id-rtd 1))
