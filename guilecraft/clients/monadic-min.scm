;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;;; Client Base Library With Monads

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

;;;; Commentary:
;;;
;;; Provide the common interfaces likely to be used by all other
;;; client implementations. These are built using the 'lounge-monad',
;;; allowing us to abstract from lounge context (tokens,
;;; lounge-connection and library-connection).
;;;
;;;; Code:

(define-module (guilecraft clients monadic-min)
  #:use-module (guilecraft config)
  #:use-module (guilecraft comtools)
  #:use-module (guilecraft utils)
  #:use-module (guilecraft monads)
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft data-types profile-requests)
  #:use-module (guilecraft data-types module-requests)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (srfi srfi-26)
  #:export (
            ;; monad helpers
            echor
            mechor
            state?
            mk-state
            state-tk
            state-lib
            state-lng
            state-lesser
            lstateful
            lstateful?
            unlstateful
            logger
            ;; state generating transactions
            register-player
            authenticate-player
            ;; composite transactions
            add-active-modules
            next-challenge
            submit-answer
            delete-player
            known-modules
            ;; atomic/monadic transactions
            fetch-known-modules
            push-deletion
            fetch-challenge-id
            fetch-challenge
            fetch-evaluation
            push-evaluation
            fetch-hashmap
            fetch-id-hash-pairs
            push-scorecard
            push-active-modules
            ;; Helper Procedures
            lesser-call/exchange
            call/exchange
            push-data
            ))

;; A monadic value in the lounge-monad context is a procedure taking
;; TOKEN, LOUNGE and LIBRARY as arguments, and carrying out an
;; exchange with the lounge through these. e.g:
(define (echor message)
  (lambda (state)
    (let ((rs (rs-content
               (exchange
                (request (echoq (car state) message)) (cadr state)))))
      (stateful `(,echos-message rs) (list (echos-token   rs)
                                           (echos-lounge  rs)
                                           (echos-library rs))))))
(define (mechor message base)
  (lambda (state)
    (let ((rs (rs-content
               (exchange
                (request (echoq (car state)
                                (string-append message base)))
                (cadr state)))))
      (stateful `(,echos-message rs) (list (echos-token   rs)
                                           (echos-lounge  rs)
                                           (echos-library rs))))))


;;;; Client Monad

;;;; A monad specialised for the client stateful tasks. It provides
;;;; state management, and logging.
(define (logger st8ful)
  (if (relevant? 'debug)
      (let ((st8  (state st8ful))
            (port (if (string? %log-file%)
                      (open-file %log-file% "a")
                      (current-output-port))))
        (format port "Value: ~a, Token: ~a, Lounge: ~a, Library: ~a.\n"
                (result st8ful) (state-tk st8) (state-lng st8)
                (state-lib st8))
        (if (string? %log-file%) (close-output-port port)))))

(define (lstateful value message st8)
  "Return a stateful with an additional log field out of VALUE,
MESSAGE and ST8."
  (list value 'log message st8))
(define (lstateful? lst8ful)
  (eqv? (car (state lst8ful)) 'log))
(define (unlstateful lst8ful)
  "Return the state of LST8FUL. Print the log message if LST8FUL
is an lstateful."
  (let ((st8 (state lst8ful)))
    (if (eqv? (car st8) 'log)
        (begin
          (apply format #t (cadr st8))
          (newline)
          (caddr st8))
        (begin
          (logger lst8ful)
          st8))))
(define (mk-state token lounge library)
  (list 'st8 token lounge library))
(define (state? st8)
  (eqv? (car st8) 'st8))
(define (state-tk state)
  "Return token from STATE."
  (cadr state))
(define (state-lng state)
  "Return the lounge connection from STATE."
  (caddr state))
(define (state-lib state)
  "Return the library connection from STATE."
  (cadddr state))
(define (state-lesser state)
  "Return token and the lounge connection from STATE."
  (list (cadr state) (caddr state)))

(define (client-return value)
  "Return a state mvalue seeded with VALUE."
  (lambda (state)
    "Return a state-pair of value and STATE."
    (stateful value state)))

(define (client-bind mvalue mproc)
  "Return a state mvalue, in turn capable of returning the result of
applying MVALUE to MPROC."
  (lambda (st8)
    ;; Generate new-state-pair by passing state into mvalue.
    (let ((new-stateful (mvalue st8)))
      (logger new-stateful) ; Use logging mechanism
      ;; Return the state-pair resulting from applying the new state
      ;; to mproc seeded with the new result.
      ((mproc (result new-stateful)) (state new-stateful)))))

(define-monad client-monad
  (bind   client-bind)
  (return client-return))

;;;; Communication Wrappers

;;; This section contains wrappers and convenience procedures for
;;; clients to communicate with guilecraft servers.  The approach has
;;; been to provide a powerful general procedure (call/exchange),
;;; which is capable of returning very precise data sets from the
;;; server, and to then provide wrappers for individual requests and
;;; their responses that use call/exchange to provide data to the
;;; client program.

;;;;; 'State' Generating Procedures

(define (register-player name profile-server module-server)
  "Return 'lounge state' upon successful registration with the
lounge using NAME. Raise an Exchange Error otherwise."
  (catch 'exchange-error
    (lambda ()
      (let ((rs (call/exchange profile-server auths? regq
                               name profile-server module-server)))
        (mk-state (auths-token rs) (auths-prof-server rs)
                  (auths-mod-server rs))))
    (lambda (key neg)
      (let ((orig (neg-orig neg)))
        (list (cadr (neg-msg neg))
              (regq-name orig)
              (regq-prof-server orig)
              (regq-mod-server orig))))))

(define (authenticate-player name profile-server)
  "Return 'lounge state' upon successful authentication with the
lounge using NAME. Raise an Exchange Error otherwise."
  (catch 'exchange-error
    (lambda ()
      (let ((rs (call/exchange profile-server auths? authq
                               name)))
        (mk-state (auths-token rs) (auths-prof-server rs)
                  (auths-mod-server rs))))
    (lambda (key neg)
      (let ((orig (neg-orig neg)))
        (list (cadr (neg-msg neg))
              (authq-name orig)
              profile-server)))))

;;;;; Composite Transactions
(define (add-active-modules module-ids state)
  "Given a set of module-ids provided, for instance, by the player
choosing from amongst a list of modules, carry out the necessary
transactions to activate these modules for the player."
  ((mlet*
    client-monad
    ;; Get sethash pairs
    ((hash-pairs (fetch-id-hash-pairs module-ids))
     ;; Update profile active modules, retrieve newly required
     ;; hashmaps.
     (req-maps   (push-active-modules (car hash-pairs)))
     ;; Get hashmaps if necessary.
     ;;FIXME: implement if clause for this.
     (hashmap    (fetch-hashmap (car req-maps))))
    ;; Update profile scorecards with hashmaps.
    ;;FIXME: only if step above required.
    (push-scorecard (car hashmap))) state))

(define (next-challenge state)
  "Given the usual STATE of token, lounge and library, return the
next-challenge for the player associated with token."
  ((mlet*
    client-monad
    ;; Get next challenge blobhash/counter
    ((challenge-details (fetch-challenge-id)))
    ;; Get next problem
    (apply fetch-challenge challenge-details)) state))

(define (submit-answer answer state)
  "Given the usual STATE of token, lounge and library, submit the
player's answer for assesment, and push the result of assesment to the
player's profile."
  ((mlet*
    client-monad
    ;; Evaluate answer
    ((challenge-details (fetch-challenge-id))
     (evaluation (apply fetch-evaluation answer
                        challenge-details))
     ;; Update profile scorecard…
     (pushed (push-evaluation (car evaluation))))
    ;; …But return evaluation result + new state
    (return evaluation)) state))

(define (delete-player state)
  "Given the usual STATE of token, lounge and library, request lounge
delete the player identified by token."
  (catch 'exchange-error
    (lambda ()
      ((push-deletion) state))
    (lambda (key neg)
      (list (cadr (neg-msg neg))
            (delq-token (neg-orig neg))))))

(define (known-modules state)
  "Given the usual STATE of token, lounge and library, request library
provides us with details of available modules."
  (catch 'exchange-error
    (lambda ()
      ((fetch-known-modules) state))
    (lambda (key neg)
      (list (cadr (neg-msg neg))))))

;;;;; Atomic Transactions / Monadic Transactions
(define (fetch-known-modules)
  "Return a client-monad mval for an availq."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               knowns? knownq           ; predicate, constructor
               ;; add search?           ; input
               )))
      (stateful (knowns-list rs)
                state))))
(define (push-deletion)
  "Return a client-monad mval for a delq request."
  (lambda (state)
    "Return a steteful whose result's first values is #t upon
successful deletion of the profile identified by the lounge server and
the token in STATE. Raise an Exchange Error otherwise."
    (let ((rs (call/exchange
               (state-lng state)        ; lounge connection
               acks? delq               ; predicate, constructor
               (state-tk state))))      ; input
      (stateful '(#t)
                (mk-state #f
                          #f
                          #f)))))

(define (fetch-challenge-id)
  "Return chauths or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lng state)        ; lounge connection
               chauths? chauthq         ; predicate, constructor
               (state-tk state))))      ; input
      (stateful `(,(chauths-hash    rs)
                  ,(chauths-counter rs))
                (mk-state (chauths-token rs)
                          (state-lng     state)
                          (state-lib     state))))))

(define (fetch-challenge blobhash blobcounter)
  "Return challs or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               challs? challq           ; predicate, constructor
               blobhash blobcounter)))  ; inputs
      (stateful `(,(challs-challenge rs))
                state))))

(define (fetch-evaluation answer blobhash blobcounter)
  "Return evals or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               evals? evalq             ; predicate, constructor
               blobhash blobcounter
               answer)))                ; inputs
      (stateful `(,(evals-result   rs)
                  ,(evals-solution rs))
                state))))

(define (push-evaluation evaluation)
  "Return evals or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lng state)        ; lounge connection
               auths? evauthq           ; predicate, constructor
               (state-tk state)         ; token
               evaluation)))            ; input
      (stateful '(unimportant)
                (mk-state (auths-token           rs)
                          (auths-prof-server     rs)
                          (auths-mod-server      rs))))))

(define (fetch-hashmap crownsets)
  "Return hashmaps or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               hashmaps? hashmapq       ; predicate, constructor
               crownsets)))             ; input
      (stateful `(,(hashmaps-content rs))
                state))))

(define (fetch-id-hash-pairs set-ids)
  "Return a sethashess or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               sethashess? sethashesq   ; predicate, constructor
               set-ids)))               ; input
      (stateful `(,(sethashess-set-ids rs))
                (mk-state (state-tk  state)
                          (state-lng state)
                          (state-lib state))))))

(define (push-scorecard hashmap)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (lambda (state)
    (let ((rs (apply push-data
                     'scorecard              ; thing to be updated,
                     hashmap                 ; input,
                     (state-lesser state)))) ; token, lounge
      (if (set!s? rs)
          (stateful `(,(set!s-value rs))
                    (mk-state (set!s-token rs)
                              (state-lng   state)
                              (state-lib   state)))
          (stateful '(unimportant)
                    (mk-state (auths-token       rs)
                              (auths-prof-server rs)
                              (auths-mod-server  rs)))))))

(define (push-active-modules id-hash-pairs)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (lambda (state)
    (let* ((rs (apply push-data
                      'active-modules         ; thing to be updated,
                      id-hash-pairs           ; input,
                      (state-lesser state)))) ; token, lounge
      (if (set!s? rs)
          (stateful `(,(set!s-value rs))
                    (mk-state (set!s-token rs)
                              (state-lng state)
                              (state-lib state)))
          (stateful 'unimportant
                    (mk-state (auths-token       rs)
                              (auths-prof-server rs)
                              (auths-mod-server  rs)))))))

;;;;; Helper Procedures
(define (push-data type data token profile-server)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (let ((result (lesser-call/exchange profile-server set!q
                                      token type data)))
    (if (or (auths? result)
            (set!s? result))
        result
        (throw 'exchange-error result))))

(define (call/exchange target predicate rq . args)
  "Return the expected request of performing an RQ with ARGS on TARGET
as validated by PREDICATE. Raise an error if the response is not
expected."
  (let ((result (apply lesser-call/exchange target rq args)))
    (if (predicate result)
        result
        (throw 'exchange-error result))))

(define (lesser-call/exchange target rq . args)
  "Return the request of performing an RQ with ARGS on TARGET. Raise
an error if the response is not expected."
  (let ((rs (exchange (request (apply rq args)) target)))
    (if rs
        (rs-content rs)
        (negs (apply rq args) '(exchange not-alive?)))))
