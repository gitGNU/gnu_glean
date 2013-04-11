;;; guilecraft --- .         -*- coding: utf-8 -*-

(define-module (guilecraft types open-problems)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (;; open-problem record functions
	    op_assess
	    op_open-problem?
	    op_make-open-problem
	    op_get-challenge
	    op_get-solution))

;;;
;; Define Problems
;;;

;;;
;; Problems are the lowest level data-type used in gmodule definition.
;; They provide a challenge and some form of correct solution.
;; We aim to provide a great variety of problem-types such as:
;; - open-problems: challenges expecting a freely typed answer that 
;; matches the provided solution
;; - multiple-choice: challenges that offer a number of solutions from
;; which the user chooses.
;; - dynamic: challenges are generated dynamically according to rules,
;; the user must type an answer that matches the algorithmically
;; generated solution.
;;
;; Problems should also be able to provide their challenges using
;; different media types: text, audio, video.

;; Open-problem: an individual problem record of the open problem
;; variety.
(define-record-type <open-problem>
  (op_make-open-problem challenge solution)
  op_open-problem?
  ; Challenges are posed to players to provide a solution to.
  (challenge op_get-challenge)      ; String
  ; Solutions are what the player's response is assessed against.
  (solution op_get-solution)        ; String 
  ) 

;; Helper Functions:
(define op_assess
  (lambda (player-answer open-problem-solution)
    "Evaluate @var{player-answer} against @var{open-problem-solution}
and return #t if correct or #f if incorrect."
    (op_eq? player-answer open-problem-solution)))

(define op_eq?
  (lambda (player-answer open-problem-solution)
    "Predicate equivalence for @var{player-answer} and
@var{open-problem-solution}. This is separated into a separate module
to maximise the resilience of the superstructure against low-level changes."
    (if (equal? player-answer open-problem-solution)
	#t
	#f)))
