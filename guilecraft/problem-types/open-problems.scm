;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft problem-types open-problems)
  #:use-module (guilecraft problem-type-manager)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (open-problem-provider
	    make-open-problem))

(define-record-type <open-problem>
  (make-problem challenge solution)
  open-problem?
  ; Challenges are posed to players to provide a solution to.
  (challenge get-challenge)      ; String
  ; Solutions are what the player's response is assessed against.
  (solution get-solution)        ; String 
  ) 

;; Helper Functions:
(define assess
  (lambda (open-problem-solution player-answer)
    "Evaluate @var{player-answer} against @var{open-problem-solution}
and return #t if correct or #f if incorrect."
    (eq? open-problem-solution player-answer)))

(define eq?
  (lambda (open-problem-solution player-answer)
    "Predicate equivalence for @var{player-answer} and
@var{open-problem-solution}. This is separated into a separate module
to maximise the resilience of the superstructure against low-level changes."
    (if (equal? open-problem-solution player-answer)
	#t
	#f)))

;; this will be the central interface providing an id method, asses
;; and get-problem interfaces, as well as possibly a print method
(define open-problem-provider
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
	   (make-problem (car args) (cadr args)))
	  ((eq? message 'print-problem)
	   (format #t "print not yet, using display:\n ~s" (car args))))))

(define make-open-problem
  (lambda (question solution)
    "Convenience procedure for creating open-problems when defining modules."
    (open-problem-provider 'make question solution)))

(ptm_add-problem-type open-problem-provider (make-problem "problem" "solution"))
