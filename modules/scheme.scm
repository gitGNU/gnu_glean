;;; -*- coding:utf-8 -*-

(define-module (modules scheme)
  #:use-module (guilecraft gmodules)
  #:export (scheme))

(define scheme 
  (make-gmodule "The Scheme Programming Language"
		"0.1"
		"A module taking the player on a whirlwind tour of the Scheme programming language."
		"Long Description: background on Scheme, introductory text"
		"Alex Sassmannshausen"
		"http://www.schemers.org"
		"Kent Dybvig's The Scheme Programming Language"
		;; Will add:
		;; - general-predicates (eq? etc.)
		;; - lists (making, using, comparing)
		;; - boolean (ibid.)
		;; - numbers (ibid.)
		;; - strings (ibid.)
		;; - constants (ibid.)
		`(,general-predicates ,lists ,numbers ,strings)))

(define general-predicates
  (make-gset 'general-predicates
	     `(,(make-open-problem
		"What would you use to compare 2 numbers? (name the most minimal predicate you can use)"
		"eq?"))))

(define lists
  (make-gset 'lists
	     `(,(make-open-problem
		"What is the predicate for the empty list?" 
		"null?"))))

(define numbers
  (make-gset 'numbers
	     `(,(make-open-problem
		"What is the predicate for numbers?" 
		"number?"))))

(define strings
  (make-gset 'strings
	     `(,(make-open-problem
		"What is the predicate for strings?" 
		"string?"))))
