;;; -*- coding:utf-8 -*-

(define-module (modules scheme)
  #:use-module (guilecraft gmodules)
  #:use-module (guilecraft gsets)
  #:use-module (guilecraft open-problems)

  #:export (scheme))

(define scheme 
  (gmod_make-gmodule 'scheme
		     "The Scheme Programming Language"
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
  (gset_make-gset 'general-predicates
		  `(,(op_make-open-problem
		      "What would you use to compare 2 numbers? (name the most minimal predicate you can use)"
		      "eq?"))))

(define lists
  (gset_make-gset 'lists
		  `(,(op_make-open-problem
		      "What is the predicate for the empty list?" 
		      "null?"))))

(define numbers
  (gset_make-gset 'numbers
		  `(,(op_make-open-problem
		      "What is the predicate for numbers?" 
		      "number?"))))

(define strings
  (gset_make-gset 'strings
		  `(,(op_make-open-problem
		      "What is the predicate for strings?" 
		      "string?"))))
