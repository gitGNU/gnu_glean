#! /usr/bin/guile
coding:utf-8
!#

;;; Evaluator evaluates the answer provided by UI for 
;;; a specific problem ID against the solution provided
;;; by Library on the basis of that ID. It evaluates
;;; on the basis of rules defined by the module (Library)
;;; and returns correct/incorrect to UI.
;;; It stores last ID and correct/incorrect to pass on
;;; to Balancer.

(define-module (guilecraft evaluator)
  #:export (correct-answer?)	; by Controller
  #:use-module (guilecraft library)) 

(define (correct-answer? problem-ID answer)
  "Compare a supplied answer to the solution of the supplied problem. Return #t or #f."
  (equal? answer (request-solution problem-ID)))
