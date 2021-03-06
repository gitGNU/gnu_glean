;; clients-min.scm --- clients-min unit tests    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
;;
;; This file is part of Glean.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Clients-min unit tests.
;;
;;; Code:

(define-module (tests clients-min)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (glean config)
  #:use-module (glean common monads)
  #:use-module (glean common base-requests)
  #:use-module (glean common lounge-requests)
  #:use-module (glean common library-requests)
  #:use-module (glean client monadic-min))

(test-begin "clients-min-tests")

;; A test token modified throughout successfull tests with set!
(define test-tk #f)
(define test-mon #f)
;; Other sideeffect vars
(define test-tmp #f)
(define test-q #f)

(test-assert "lesser-call/exchange"
             (negs?
              (lesser-call/exchange
               %lounge-port%    ; target
               echoq                    ; request
               'token-lc/e 'msg-lc/e))) ; inputs
(test-assert "call/exchange"
             (negs?
              (call/exchange
               %lounge-port%    ; target
               negs? echoq              ; predicate, constructor
               'token-c/e 'msg-c/e)))   ; inputs
;; Lounge Test: Token update
(test-assert "register-player"
             (let ((st8 (register-player "test"
                                         %lounge-port%
                                         %library-port%)))
               (if (state? st8)
                   (begin
                     (set! test-tk (state-tk st8))
                     #t)
                   #f)))
;; Library Test: Tmp update
(test-assert "fetch-id-hash-pairs"
             (let ((st8ful ((fetch-id-hash-pairs '(test))
                            (mk-state test-tk %lounge-port%
                                      %library-port%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tmp (car (result st8ful)))
                          #t)
                   #f)))
;; Lounge Test: Token and Tmp update
(test-assert "push-active-modules"
             (let ((st8ful ((push-active-modules test-tmp)
                            (mk-state test-tk %lounge-port%
                                      %library-port%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tk  (state-tk (state st8ful)))
                          (set! test-tmp (car (result st8ful)))
                          #t)
                   #f)))
;; Library Test: Tmp update
(test-assert "fetch-hashmap"
             (let ((st8ful ((fetch-hashmap test-tmp)
                            (mk-state test-tk
                                      %lounge-port%
                                      %library-port%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tmp (car (result st8ful)))
                          #t)
                   #f)))
;; Lounge Test: Token update
(test-assert "push-scorecard"
             (let ((st8ful ((push-scorecard test-tmp)
                            (mk-state test-tk %lounge-port%
                                      %library-port%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tk (state-tk (state st8ful)))
                          #t)
                   #f)))
;; Lounge Test: Token and Tmp update
(test-assert "fetch-challenge-id"
             (let ((st8ful ((fetch-challenge-id)
                            (mk-state test-tk %lounge-port%
                                      %library-port%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tk  (state-tk (state st8ful)))
                          (set! test-tmp (result st8ful))
                          #t)
                   #f)))
;; Library Test: Q update
(test-assert "fetch-challenge"
             (let ((st8ful ((apply fetch-challenge test-tmp)
                            (mk-state test-tk %lounge-port%
                                      %library-port%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-q (car (result st8ful)))
                          #t)
                   #f)))
;; Library Test: Tmp update
(test-assert "fetch-evaluation"
             (let ((st8ful ((apply fetch-evaluation "test"
                                   test-tmp)
                            (mk-state test-tk %lounge-port%
                                      %library-port%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tmp (car (result st8ful)))
                          #t)
                   #f)))
;; Lounge Test: Token
(test-assert "push-evaluation"
             (let ((st8ful ((push-evaluation test-tmp)
                            (mk-state test-tk %lounge-port%
                                      %library-port%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tk  (state-tk (state st8ful)))
                          #t)
                   #f)))
;; Lounge Test: Delete Test Account
(test-assert "delete-user"
             (let ((st8ful ((push-deletion)
                            (mk-state test-tk %lounge-port%
                                      %library-port%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tk  (state-tk (state st8ful)))
                          #t)
                   #f)))
;; Monadic Transaction Test: Active modules
(format #t "\nMonadic Test.\n")
(test-assert "add-active-modules"
             (let ((st8ful
                    (add-active-modules
                     '(test)
                     (register-player "mon-test"
                                      %lounge-port%
                                      %library-port%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-mon (state st8ful))
                          #t)
                   #f)))
;; Monadic Transaction Test: Request Challenge
(test-assert "next-challenge"
             (let ((st8ful (next-challenge test-mon)))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-mon (state st8ful))
                          #t)
                   #f)))
;; Monadic Transaction Test: Push Evaluation
(test-assert "submit-answer"
             (let ((st8ful (submit-answer "test" test-mon)))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-mon (state st8ful))
                          #t)
                   #f)))
;; Monadic Transaction Test: Delete User
(test-assert "delete-player"
             (let ((st8ful (delete-player test-mon)))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-mon (state st8ful))
                          #t)
                   #f)))

(test-end "clients-min-tests")

;;; discipline ends here
