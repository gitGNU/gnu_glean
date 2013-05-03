;;; guilecraft --- .         -*- coding: utf-8 -*-

(define-module (guilecraft problem-types multi-choice-problems)
  #:use-module (guilecraft data-manager)
  #:use-module (guilecraft problem-type-manager)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (multi-choice-problem-provider
	    make-multi-choice-problem))

(define-record-type <multi-choice-problem>
  (make-problem question solution options)
  multi-choice-problem?
  (question get-question)
  (solution get-solution)
  (options get-options))

(define get-challenge
  (lambda (multi-choice-problem)
    (string-join (list (get-question multi-choice-problem)
		       (turn-options (get-options multi-choice-problem))))))

(define turn-options
  (lambda (options)
    "options is a list containing all answer options. Turn options
walks through that list and creates a string formatted for output."
    (define turn-option
      (lambda (option)
	(string-append (get-option-symbol option) ") "
		       (get-option-description option) "\n")))
    (string-join (map turn-option options))))

;; Helper Functions:
(define assess
  (lambda (player-answer multi-choice-problem-solution)
    "Evaluate @var{player-answer} against @var{multi-choice-problem-solution}
and return #t if correct or #f if incorrect."
    (mcp_equal? player-answer multi-choice-problem-solution)))
(define make-option
  (lambda (symbol description)
    "Create an option used to populate the option fields in <multi-choice-problem>"
    (cons symbol description)))
(define get-option-symbol
  (lambda (option)
    "Return the symbol associated with a multi-choice-option."
    (car option)))
(define get-option-description
  (lambda (option)
    (cdr option)))

(define mcp_equal?
  (lambda (player-answer multi-choice-problem-solution)
    "Predicate equivalence for @var{player-answer} and
@var{multi-choice-problem-solution}. This is separated into a separate module
to maximise the resilience of the superstructure against low-level changes."
    (if (or (equal? player-answer (get-option-symbol multi-choice-problem-solution))
	    (equal? player-answer (get-option-description multi-choice-problem-solution)))
	#t
	#f)))

;; this will be the central interface providing an id method, asses
;; and get-problem interfaces, as well as possibly a print method
(define multi-choice-problem-provider
  (lambda (message . args)
    "Provide an open problem type interface.  Inserted into
problem-type-manager, this procedure provides all actions that need to
be provided by a problem type.

The car of args should always be the problem."
    (cond ((eq? message 'get-challenge)
	   (get-challenge (car args)))
	  ((eq? message 'assess-answer)
	   (assess (cadr args) (get-solution (car args))))
	  ((eq? message 'make)
	   (make-problem (car args)  ; question
			 (cadr args) ; solution
			 (car (cddr args)) ; options
			 ))
	  ((eq? message 'print-problem)
	   (format #t "print not yet, using display:\n ~s" (car args))))))

(define make-multi-choice-problem
  (lambda (question solution . options)
    "Convenience procedure for creating multi-choice-problems when defining modules."
    (multi-choice-problem-provider 'make question solution options)))

(ptm_add-problem-type multi-choice-problem-provider (make-problem
						     "question"
						     "solution"
						     '("option1"
						       "option2"
						       "option3")))
