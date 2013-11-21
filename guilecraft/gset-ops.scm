;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft gset-ops)
  #:use-module (guilecraft data-types sets)
  #:export (get-tag-problems
	    first-contents
	    rest-contents))

(define (get-tag-problems id set)
  "Return the problems subsumed under ID in a given SET or '() if
no problems exist for ID in SET."
  (define (helper sets)
    (cond ((null? sets)
	   '())
	  ((eqv? id (set-id (car sets)))
	   (set-contents (car sets)))
	  (else (helper (cdr sets)))))
  (helper (set-contents set)))

(define (first-content set)
  "Return the first element in the contents list of SET."
  (car (set-contents set)))

(define (rest-contents set)
  "Return the rest of the contents list of SET."
  (cdr (set-contents set)))
