;;; glean --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (store test)
  #:use-module (glean library core-templates)
  #:export (test-module))

(define test-module
  (module
    'test
    #:name "A Test Module"
    #:version "0.1"
    #:keywords '("test" "simple" "open" "multiple-choice")
    #:synopsis "This is a bare skeleton of a module."
    #:description "The aim is to have a module that:
a) illustrates fields and their acceptable values;
b) provides a module with fields to be used by the unit tests."
    #:creator "Alex Sassmannshausen"
    #:attribution
    (list
     (media #:text "We did not need inspiration for this one"))
    #:resources
    (list
     (media #:text "But if you want to find out more, try:"
	    #:books (list "The Glean Manual")))
    #:contents
    (list (set 'intro
	       #:contents
	       (list (problem (q "What is a good question?")
			      (s "This is not")
			      (o "This is not")
			      (o "This is")
			      (o "What is your favourite colour")
			      (p equal?))
		     (problem (q "An open question should…")
			      (s "…have a single obvious solution")
			      ;; (s "…be written with clarity in mind")
			      (o "…have a single obvious solution")
			      (o "…be written with clarity in mind")
			      (o "…have multiple interprations")
			      (o "…use mystifying language")))))))
