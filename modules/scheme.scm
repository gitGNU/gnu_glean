;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (modules scheme)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft problem-types open-problems)
  #:use-module (guilecraft problem-types multi-choice-problems)
  #:use-module (guilecraft gmodule-manager)
  #:export (scheme-gmodule))

(define scheme-gmodule
  (gmodule
   (id 'scheme)
   (name  "The Scheme Programming Language")
   (version "0.1")
   (description "A module taking the player on a whirlwind tour of the Scheme programming language.")
   (long-description "Long Description: background on Scheme, introductory text")
   (creators "Alex Sassmannshausen")
   (find-out-more "http://www.schemers.org")
   (derivation-source "Kent Dybvig's The Scheme Programming Language")
   (parts `(,general-predicates ,lists ,numbers ,strings))))
;; Will add:
;; - general-predicates (eq? etc.)
;; - lists (making, using, comparing)
;; - boolean (ibid.)
;; - numbers (ibid.)
;; - strings (ibid.)
;; - constants (ibid.)

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

(gman_add-gmodule scheme-gmodule)
