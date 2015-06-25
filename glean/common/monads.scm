;; monads.scm --- general monad library            -*- coding: utf-8 -*-
;;
;; Copyright (C) 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
;; Functionality providing foundational monad logic. The library also
;; exposes procedures for a state, ‘Just‘ and an identity monad.
;;
;; The commentary from Guix's monads.scm (which this module is largely based
;; on):
;; This module implements the general mechanism of monads, and provides in
;; particular an instance of the "store" monad.  The API was inspired by that
;; of Racket's "better-monads" module (see
;; <http://planet.racket-lang.org/package-source/toups/functional.plt/1/1/planet-docs/better-monads-guide/index.html>).
;; The implementation and use case were influenced by Oleg Kysielov's
;; "Monadic Programming in Scheme" (see
;; <http://okmij.org/ftp/Scheme/monad-in-Scheme.html>).
;;
;;; Code:

(define-module (glean common monads)
  #:use-module (glean common utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module ((system syntax)
                #:select (syntax-local-binding))
  #:export (;; Monads.
            define-monad
            monad?
            monad-bind
            monad-return

            ;; Syntax.
            >>=
            return
            with-monad
            mlet
            mlet*
            lift1 lift2 lift3 lift4 lift5 lift6 lift7 lift
            listm
            foldm
            mapm
            sequence
            anym

            ;; Concrete monads.
            identity-monad
            state-monad
            stateful
            stateful?
            result
            state
            mk-state
            state?
            null-state
            null-state?
            state-tk
            set-state-tk
            state-lng
            set-state-lng
            state-lib
            set-state-lib
            state-format
            set-state-format
            state-serialize
            state-unserialize
            mlogger
            )
  #:replace (imported-modules
             compiled-modules))


;;;;; Basic Monad Infrastructure

;;; Record type for monads manipulated at run time.
(define-record-type <monad>
  (make-monad bind return)
  monad?
  (bind   monad-bind)
  (return monad-return))                ; TODO: Add 'plus' and 'zero'

(define-syntax define-monad
  (lambda (s)
    "Define the monad under NAME, with the given bind and return
methods."
    (define prefix (string->symbol "% "))
    (define (make-rtd-name name)
      (datum->syntax name
                     (symbol-append prefix
                                    (syntax->datum name) '-rtd)))

    (syntax-case s (bind return)
      ((_ name (bind b) (return r))
       (with-syntax ((rtd (make-rtd-name #'name)))
         #`(begin
             (define rtd
               ;; The record type, for use at run time.
               (make-monad b r))

             (define-syntax name
               ;; An "inlined record", for use at expansion time.  The
               ;; goal is to allow 'bind' and 'return' to be resolved
               ;; at expansion time, in the common case where the
               ;; monad is accessed directly as NAME.
               (lambda (s)
                 (syntax-case s (%bind %return)
                   ((_ %bind)   #'b)
                   ((_ %return) #'r)
                   (_           #'rtd))))))))))

(define-syntax-parameter >>=
  ;; The name 'bind' is already taken, so we choose this (obscure) symbol.
  (lambda (s)
    (syntax-violation '>>= ">>= (bind) used outside of 'with-monad'" s)))

(define-syntax-parameter return
  (lambda (s)
    (syntax-violation 'return "return used outside of 'with-monad'" s)))

(define-syntax with-monad
  (lambda (s)
    "Evaluate BODY in the context of MONAD, and return its result."
    (syntax-case s ()
      ((_ monad body ...)
       (eq? 'macro (syntax-local-binding #'monad))
       ;; MONAD is a syntax transformer, so we can obtain the bind and return
       ;; methods by directly querying it.
       #'(syntax-parameterize ((>>=    (identifier-syntax (monad %bind)))
                               (return (identifier-syntax (monad %return))))
           body ...))
      ((_ monad body ...)
       ;; MONAD refers to the <monad> record that represents the monad at run
       ;; time, so use the slow method.
       #'(syntax-parameterize ((>>=    (identifier-syntax
                                        (monad-bind monad)))
                               (return (identifier-syntax
                                        (monad-return monad))))
           body ...)))))


;;;;; Basic Monad Use

(define-syntax mlet*
  (syntax-rules (->)
    "Bind the given monadic values MVAL to the given variables VAR.  When the
form is (VAR -> VAL), bind VAR to the non-monadic value VAL in the same way as
'let'."
    ;; Note: the '->' symbol corresponds to 'is:' in 'better-monads.rkt'.
    ((_ monad () body ...)
     (with-monad monad body ...))
    ((_ monad ((var mval) rest ...) body ...)
     (with-monad monad
       (>>= mval
            (lambda (var)
              (mlet* monad (rest ...)
                body ...)))))
    ((_ monad ((var -> val) rest ...) body ...)
     (let ((var val))
       (mlet* monad (rest ...)
         body ...)))))

(define-syntax mlet
  (lambda (s)
    (syntax-case s ()
      ((_ monad ((var mval ...) ...) body ...)
       (with-syntax (((temp ...) (generate-temporaries #'(var ...))))
         #'(mlet* monad ((temp mval ...) ...)
             (let ((var temp) ...)
               body ...)))))))

(define-syntax define-lift
  (syntax-rules ()
    ((_ liftn (args ...))
     (define (liftn proc monad)
       "lift proc to monad---i.e., return a monadic function in monad."
       (lambda (args ...)
         (with-monad monad
           (return (proc args ...))))))))

(define-lift lift1 (a))
(define-lift lift2 (a b))
(define-lift lift3 (a b c))
(define-lift lift4 (a b c d))
(define-lift lift5 (a b c d e))
(define-lift lift6 (a b c d e f))
(define-lift lift7 (a b c d e f g))

(define (lift nargs proc monad)
  "lift proc, a procedure that accepts nargs arguments, to monad---i.e.,
return a monadic function in monad."
  (lambda args
    (with-monad monad
      (return (apply proc args)))))


;;;;; Common Higher Order Functions For Monads

(define (foldm monad mproc init lst)
  "fold mproc over lst, a list of monadic values in monad, and return a
monadic value seeded by init."
  (with-monad monad
    (let loop ((lst    lst)
               (result init))
      (match lst
        (()
         (return result))
        ((head tail ...)
         (mlet* monad ((item   head)
                       (result (mproc item result)))
           (loop tail result)))))))

(define (mapm monad mproc lst)
  "map mproc over lst, a list of monadic values in monad, and return a monadic
list."
  (foldm monad
         (lambda (item result)
           (mlet monad ((item (mproc item)))
             (return (cons item result))))
         '()
         (reverse lst)))

(define-inlinable (sequence monad lst)
  "turn the list of monadic values lst into a monadic list of values, by
evaluating each item of lst in sequence."
  (with-monad monad
    (mapm monad return lst)))

(define (anym monad proc lst)
  "apply proc to the list of monadic values lst; return the first value,
lifted in monad, for which proc returns true."
  (with-monad monad
    (let loop ((lst lst))
      (match lst
        (()
         (return #f))
        ((head tail ...)
         (mlet* monad ((value  head)
                       (result -> (proc value)))
           (if result
               (return result)
               (loop tail))))))))

;;;;; A Monadic List Generator
(define-syntax listm
  (lambda (s)
    "return a monadic list in monad from the monadic values mval."
    (syntax-case s ()
      ((_ monad mval ...)
       (with-syntax (((val ...) (generate-temporaries #'(mval ...))))
         #'(mlet monad ((val mval) ...)
             (return (list val ...))))))))


;;;;; Specific Monads

;;;; The Identity Monad

(define-inlinable (identity-return value)
  value)

(define-inlinable (identity-bind mvalue mproc)
  (mproc mvalue))

(define-monad identity-monad
  (bind   identity-bind)
  (return identity-return))

;;;; The State Monad

;;; In the context of the state monad:
;;;
;;; MPROC: returns an MVALUE seeded with values passed to MPROC as
;;; it's arguments.
;;;
;;; MVALUE: returns a procedure taking state, which in turn, when
;;; provided with state, returns a stateful.

(define-record-type <stateful>
  (stateful res st8)
  stateful?
  (res result)
  (st8 state))
(define-immutable-record-type <st8>
  (mecha-state token lounge library format)
  state?
  (token   state-tk     set-state-tk)
  (lounge  state-lng    set-state-lng)
  (library state-lib    set-state-lib)
  (format  state-format set-state-format))

(define (result? thing) (list? thing))

(define (stateful-serialize st8f format)
  (match format
    ('records st8f)
    ('sxml `(stateful (result ,(result st8f))
                      ,(state-serialize (state st8f) format)))
    (_     (throw 'glean-logic-error "STATEFUL-SERIALIZE: unexpected format"
                  format))))

(define* (mk-state token lounge library #:optional (format 'records))
  (mecha-state token lounge library format))

(define* (null-state #:optional (lounge "") (library "") (format 'records))
  (mk-state 0 lounge library format))

(define (null-state? st8)
  (match st8
    (($ <st8> 0 (? string?) (? string?) (? symbol?)) #t)
    (_                                               #f)))

(define (state-serialize st8 format)
  (match format
    ('records st8)
    ('sxml `(state (token   ,(state-tk st8))
                   (lounge  ,(state-lng st8))
                   (library ,(state-lib st8))
                   (format  ,(state-format st8))))
    (_     (throw 'glean-logic-error "STATE-SERIALIZE: unexpected format"
                  format))))

(define (state-unserialize st8 format)
  (match format
    ('records st8)
    ('sxml    (match st8
                (('state ('token   tk)
                         ('lounge  lng)
                         ('library lib)
                         ('format  form))
                 (mk-state (inexact->exact tk) lng lib form))))
    (_        (throw 'glean-logic-error "STATE-UNSERIALIZE: unexpected format"
                     format))))

(define (state-return value)
  "Return a state mvalue seeded with VALUE."
  (lambda (state)
    "Return a state-pair of value and STATE."
    (stateful value state)))

(define (state-bind mvalue mproc)
  "Return a state mvalue, in turn capable of returning the result of
applying MVALUE to MPROC."
  (lambda (st8)
    ;; Generate new-state-pair by passing state into mvalue.
    (let ((new-stateful (mvalue st8)))
      ;; Return the state-pair resulting from applying the new state to
      ;; mproc seeded with the new result. 
      ((mproc (result new-stateful)) (state new-stateful)))))

(define-monad state-monad
  (bind   state-bind)
  (return state-return))

(define (state-unmonad mvalue)
  "Procedure for testing monad laws."
  (car (mvalue 'state)))

;;; ===
;;; Example State Monad Application:
;;; ===
;;;
;;; Given:
;;; (define (sf v)
;;;   (lambda (state)
;;;     (cons (* v state) (+ v state))))
;;;
;;; ((mlet* state-monad
;;;         ((last (sf 7))
;;;          (penult (sf last)))
;;;         ;; second    init
;;;         (sf penult)) 20)
;;; $83 = (631260 . 3947)
;;;
;;; is equivalent to
;;;
;;; init second      penult      last
;;; (    (state-bind (state-bind (sf 7) sf) sf) 20)
;;; $84 = (631260 . 3947)
;;;
;;; I do not even know how to start thinking of this without the monad!


;;;; Logging for monads
;;;
;;; mlogger implements an approach to logging in monads.  It takes a predicate
;;; and a dictionary, after which, for every call to it (for instance from
;;; within a monadic bind procedure), it will apply the predicate to supplied
;;; values and if it passes, pass the value to the dictionary.
;;;
;;; The dictionary should as a result return 3 values:
;;; - source (e.g. procedure name of the procedure causing the value currently
;;;   being logged)
;;; - message (e.g. an informative string)
;;; - value (e.g. a value to assist in interpreting the situation)

(define (mlogger predicate dictionary-proc)
  "Return a procedure which, when invoked, will return messages according to
patterns looked up against DICTIONARY-PROC."
  ;; logger's proc returns #f when logging's disabled: no need to compose nice
  ;; messages if logging is disabled anyway.
  (if ((logger) 'test '(""))
      (lambda (obj level)
        "Print a meaningful log message derived from the context of the
monadic stateful."
        (if (predicate obj)
            (match (dictionary-proc obj level)
              ((src msg val)
               (inform "[~a]\t~a ~a.~%"  src msg val))
              (_ (exclaim "MLOGGER: dictionary returned invalid message.")))
            (exclaim "MLOGGER: object does not conform to predicate:~%  ~s."
                     obj)))
      (const #f)))


;;;; Monad Tests
;;; Monads have to obey 3 formal rules, as well as doing what you
;;; need them to do. The rules are 'left identitiy', 'right
;;; identity' and 'associativity'. The procedures in this section
;;; expose these rules in a simple fashion as a basis for a test
;;; suite.

(define (left-identity? monad unmonad)
  (let ((number (random 777)))
    (with-monad monad
      (define (f x)
        (return (* (1+ x) 2)))
      (= (unmonad (>>= (return number) f))
         (unmonad (f number))))))

(define (right-identity? monad unmonad)
  (with-monad monad
    (let ((number (return (random 777))))
      (= (unmonad (>>= number return))
         (unmonad number)))))

(define (associative? monad unmonad)
  (with-monad monad
    (define (f x)
      (return (+ 1 x)))
    (define (g x)
      (return (* 2 x)))
    (let ((number (return 85467)))
      (equal? (unmonad (>>= (>>= number f) g))
              (unmonad (>>= number (lambda (x) (>>= (f x) g))))))))

;;; monads.scm end here
