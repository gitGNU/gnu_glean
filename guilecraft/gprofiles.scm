;;; guilecraft --- user profile data-types.         -*- coding: utf-8 -*-

(define-module (guilecraft gprofiles)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-profile
	    profile?
	    get-UUID
	    get-name
	    get-active-modules
	    get-scorecard

	    make-scorecard-datum
	    get-scorecard-datum-set-tag
	    get-scorecard-datum-module
	    make-dummy-scorecard-datum

	    first-active-module
	    rest-active-modules
	    empty-active-modules?
	    active-module?

	    first-in-scorecard
	    rest-of-scorecard
	    end-of-scorecard?

	    lower-score?))

(define-record-type <profile>
  (make-profile UUID name active-modules scorecard)
  profile?
  (UUID get-UUID)	       ; Unique profile identifier; timestamp?
  (name get-name)	       ; Human friendly name
  (active-modules get-active-modules) ; List of currently active gmodules
  (scorecard get-scorecard))	      ; List of gset counters for
					; currently used gsets
(define-record-type <scorecard-datum>
  (make-scorecard-datum module set-tag score)
  scorecard-datum?
  (module get-scorecard-datum-module)
  (set-tag get-scorecard-datum-set-tag)
  (score get-scorecard-datum-score))

(define make-dummy-scorecard-datum
  (lambda ()
    (make-scorecard-datum 'no-module 'no-set-tag #f)))

(define dummy-scorecard-datum?
  (lambda (scorecard-datum)
    (if (and (eq? (get-scorecard-datum-module scorecard-datum)
		  'no-module)
	     (eq? (get-scorecard-datum-set-tag scorecard-datum)
		  'no-set-tag)
	     (not (get-scorecard-datum-score scorecard-datum)))
	#t
	#f)))

(define first-active-module
  (lambda (active-modules)
    (car active-modules)))

(define rest-active-modules
  (lambda (active-modules)
    (cdr active-modules)))

(define empty-active-modules?
  (lambda (active-modules)
    (null? active-modules)))

(define active-module?
  (lambda (scorecard-datum active-modules)
    (member (get-scorecard-datum-module scorecard-datum) 
	    active-modules)))

(define first-in-scorecard
  (lambda (scorecard)
    (car scorecard)))

(define rest-of-scorecard
  (lambda (scorecard)
    (cdr scorecard)))

(define end-of-scorecard?
  (lambda (scorecard)
    (null? scorecard)))

(define lower-score?
  (lambda (scorecard-datum1 scorecard-datum2)
    (cond ((dummy-scorecard-datum? scorecard-datum2)
	   #t)
	  ((< (get-scorecard-datum-score scorecard-datum1) 
	      (get-scorecard-datum-score scorecard-datum2))
	   #t)
	  (else #f))))

;;; Player profile scores are stored as an association list embedded
;;; within an association list, e.g:
;;; (
;;; (git-gmodule . (git-branch . 0)
;;;                (git-log . 5))
;;; (scheme-gmodule . (scheme-lists . 3)
;;;                   (scheme-lambda .1))
;;;                   )
;;; Would return git-branch as the next challenge.
