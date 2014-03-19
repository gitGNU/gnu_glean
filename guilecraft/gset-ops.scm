;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft gset-ops)
  #:use-module (guilecraft data-types sets)
  #:use-module (rnrs)
  #:export (get-tag-problems
	    first-contents
	    rest-contents
	    set-id->hash
	    set-path->hash))

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

;;;;; Hashing

(define (set-id->hash set-id)
  (if (symbol? set-id)
      (symbol-hash set-id)
      (assertion-violation
       'set-id->hash
       "SET-ID is not a module-id."
       set-id)))

(define (set-path->hash set-path)
  (string-hash
   (fold-left (lambda (previous sym)
		(string-append previous "-" (symbol->string sym)))
	      (string-append (symbol->string (car set-path)))
	      (cdr set-path))))
