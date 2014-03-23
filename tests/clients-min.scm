;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;; Copyright (C) 2008, 2010, 2012 Alex Sassmannshausen

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(define-module (tests clients-min)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (guilecraft config)
  #:use-module (guilecraft monads)
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft data-types profile-requests)
  #:use-module (guilecraft data-types module-requests)
  #:use-module (guilecraft clients monadic-min))

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
               %profile-socket-file%    ; target
               echoq                    ; request
               'token-lc/e 'msg-lc/e))) ; inputs
(test-assert "call/exchange"
	     (negs?
              (call/exchange
               %profile-socket-file%    ; target
               negs? echoq              ; predicate, constructor
               'token-c/e 'msg-c/e)))   ; inputs
;; Lounge Test: Token update
(test-assert "register-player"
             (let ((st8 (register-player "test"
                                         %profile-socket-file%
                                         %module-socket-file%)))
               (if (state? st8)
                   (begin
                     (set! test-tk (state-tk st8))
                     #t)
                   #f)))
;; Library Test: Tmp update
(test-assert "fetch-id-hash-pairs"
             (let ((st8ful ((fetch-id-hash-pairs '(test))
                            (mk-state test-tk %profile-socket-file%
                                      %module-socket-file%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tmp (car (result st8ful)))
                          #t)
                   #f)))
;; Lounge Test: Token and Tmp update
(test-assert "push-active-modules"
             (let ((st8ful ((push-active-modules test-tmp)
                            (mk-state test-tk %profile-socket-file%
                                      %module-socket-file%))))
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
                                      %profile-socket-file%
                                      %module-socket-file%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tmp (car (result st8ful)))
                          #t)
                   #f)))
;; Lounge Test: Token update
(test-assert "push-scorecard"
             (let ((st8ful ((push-scorecard test-tmp)
                            (mk-state test-tk %profile-socket-file%
                                      %module-socket-file%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tk (state-tk (state st8ful)))
                          #t)
                   #f)))
;; Lounge Test: Token and Tmp update
(test-assert "fetch-challenge-id"
             (let ((st8ful ((fetch-challenge-id)
                            (mk-state test-tk %profile-socket-file%
                                      %module-socket-file%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tk  (state-tk (state st8ful)))
                          (set! test-tmp (result st8ful))
                          #t)
                   #f)))
;; Library Test: Q update
(test-assert "fetch-challenge"
             (let ((st8ful ((apply fetch-challenge test-tmp)
                            (mk-state test-tk %profile-socket-file%
                                      %module-socket-file%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-q (car (result st8ful)))
                          #t)
                   #f)))
;; Library Test: Tmp update
(test-assert "fetch-evaluation"
             (let ((st8ful ((apply fetch-evaluation "test"
                                   test-tmp)
                            (mk-state test-tk %profile-socket-file%
                                      %module-socket-file%))))
               (if (stateful? st8ful)
                   (begin (logger st8ful)
                          (set! test-tmp (car (result st8ful)))
                          #t)
                   #f)))
;; Lounge Test: Token
(test-assert "push-evaluation"
             (let ((st8ful ((push-evaluation test-tmp)
                            (mk-state test-tk %profile-socket-file%
                                      %module-socket-file%))))
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
                                      %profile-socket-file%
                                      %module-socket-file%))))
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
(test-end "clients-min-tests")
