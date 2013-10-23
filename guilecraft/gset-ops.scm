;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft gset-ops)
  #:use-module (guilecraft data-types sets)
  #:export (get-tag-problems))

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
