;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;;; Client Base Library With Monads

;; Copyright © 2014 Alex Sassmannshausen
;;
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
            ;; state generating transactions
            register-player
            authenticate-player
            ;; composite transactions
            view-player
            add-active-modules
            next-challenge
            submit-answer
            delete-player
            known-modules
            view-set
            ))

;; A monadic value in the client-monad context is a procedure taking
;; TOKEN, LOUNGE and LIBRARY as arguments, and carrying out an
;; exchange with the lounge through these.
;;
;; Echor and Mechor server to test: they send a message to the lounge
;; and receive the message echoed if authentication succeeded.
(define (echor message)
  (lambda (state)
    (let ((rs (rs-content
               (exchange
                (request (echoq (car state) message))
                (cadr state)))))
      (stateful (echos-message rs) (list (echos-token   rs)
                                           (echos-lounge  rs)
                                           (echos-library rs))))))
(define (mechor message base)
  (lambda (state)
    (let ((rs (rs-content
               (exchange
                (request (echoq (car state)
                                (string-append message base)))
                (cadr state)))))
      (stateful (echos-message rs) (list (echos-token   rs)
                                           (echos-lounge  rs)
                                           (echos-library rs))))))


;;;; Client Monad

;;;; A monad specialised for the client stateful tasks. It provides
;;;; state management, exception handling and logging.
(define (logger st8ful)
  (if (relevant? 'debug)
      (let ((port (if (string? %log-file%)
                      (open-file %log-file% "a")
                      (current-output-port))))
        (format port "~a.\n" (object->string st8ful))
        (if (string? %log-file%) (close-output-port port)))))

(define (state-lesser state)
  "Return token and the lounge connection from STATE."
  (list (state-tk state) (state-lng state)))

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
      (logger new-stateful)         ; Use logging mechanism
      ;; Return the state-pair resulting from applying the new
      ;; state to mproc seeded with the new result.
      (if (nothing? new-stateful)
          new-stateful
          ((mproc (result new-stateful)) (state new-stateful))))))

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

(define (register-player name password profile-server module-server)
  "Return 'lounge state' upon successful registration with the
lounge using NAME. Raise an Exchange Error otherwise."
  (let ((rs (call/exchange profile-server auths? regq
                           name password profile-server
                           module-server)))
    (if (nothing? rs)
        rs
        (mk-state (auths-token rs) (auths-prof-server rs)
                  (auths-mod-server rs)))))

(define (authenticate-player name password profile-server)
  "Return 'lounge state' upon successful authentication with the
lounge using NAME. Raise an Exchange Error otherwise."
  (let ((rs (call/exchange profile-server auths? authq
                           name password)))
    (if (nothing? rs)
        rs
        (mk-state (auths-token rs) (auths-prof-server rs)
                  (auths-mod-server rs)))))

;;;;; Composite Transactions
(define (view-set fullhash state)
  "Return a user-friendly list of full set fields as provided by the
details response, for the set identified by FULLHASH."
  ((mlet* client-monad
          ((test         (test-servers 'library))
           (detail       (fetch-detail fullhash)))
          (return detail)) state))
(define (view-player state)
  "Return a user-friendly list of profile fields for display for the
profile identified by the token in STATE."
  ((mlet* client-monad
          ((test         (test-servers))
           (details      (fetch-profile))
           (full-details (details->full-details (car details))))
          (return full-details)) state))

(define (add-active-modules fullhashes state)
  "Given a set of FULLHASHES provided, for instance, by the player
choosing from amongst a list of modules, carry out the necessary
transactions to activate these modules for the player."
  ((mlet*
       client-monad
       ((test       (test-servers))
        ;; Get set-hashpairs (minhash . fullhash)
        (hashpairs  (fetch-hashpairs fullhashes))
        ;; Update profile active modules, retrieve newly required
        ;; hashmaps.
        (req-maps   (push-active-modules (car hashpairs)))
        ;; Get hashmaps if necessary.
        (hashmap    (if (eqv? req-maps 'unimportant)
                        (return req-maps)
                        (fetch-hashmap (car req-maps)))))
       ;; Update profile scorecards with hashmaps.
       (if (eqv? hashmap 'unimportant)
           (return hashmap)
           (push-scorecard (car hashmap)))) state))

(define (next-challenge state)
  "Given the usual STATE of token, lounge and library, return the
next-challenge for the player associated with token."
  ((mlet*
    client-monad
    ((test              (test-servers))
     ;; Get next challenge blobhash/counter
     (challenge-details (fetch-challenge-id)))
    ;; Get next problem
    (apply fetch-challenge challenge-details)) state))

(define (submit-answer answer state)
  "Given the usual STATE of token, lounge and library, submit the
player's answer for assesment, and push the result of assesment to the
player's profile."
  ((mlet*
    client-monad
    ((test              (test-servers))
     ;; Evaluate answer
     (challenge-details (fetch-challenge-id))
     (evaluation (apply fetch-evaluation answer
                        challenge-details))
     ;; Update profile scorecard…
     (pushed (push-evaluation (car evaluation))))
    ;; …But return evaluation result + new state
    (return evaluation)) state))

(define (delete-player state)
  "Given the usual STATE of token, lounge and library, request lounge
delete the player identified by token."
  ((mlet*
    client-monad
    ((test (test-servers)))
    (push-deletion)) state))

(define (known-modules state)
  "Given the usual STATE of token, lounge and library, request library
provides us with details of available modules."
  ((mlet*
    client-monad
    ((test (test-servers 'library)))
    (fetch-known-modules)) state))

;;;;; Atomic Transactions / Monadic Transactions
(define* (test-servers #:optional (server #f))
  "Return a client-monad mval which returns a nothing if one of the
servers is down. If SERVER is 'library or 'lounge, only test that
server."
  (lambda (state)
    (cond ((and (or (eqv? server 'library)
                    (not server))
                (not (alive? (state-lib state))))
           (nothing 'library-down (state-lib state)))
          ((and (or (eqv? server 'lounge)
                    (not server))
                (not (alive? (state-lng state))))
           (nothing 'lounge-down (state-lng state)))
          (else (stateful '(unimportant)
                          state)))))
(define (fetch-known-modules)
  "Return a client-monad mval for an availq."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               knowns? knownq           ; predicate, constructor
               #f #f                    ; input
               )))
      (if (nothing? rs)
          rs
          (stateful (knowns-list rs)
                    state)))))
(define (fetch-detail fullhash)
  "Return a client-monad mval which, when invoked, returns details
about the set identified by FULLHASH or nothing."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               details? detailq         ; predicate, constructor
               fullhash                 ; input
               )))
      (if (nothing? rs)
          rs
          (stateful (details-list rs)
                    state)))))
(define (details->full-details details)
  "Return a client-monad mval, resolving to new DETAILS with
human-friendly information about active-modules."
  (lambda (state)
    (let ((fullhashes (map cdr (cadddr details))))
      (let ((rs (call/exchange
                 (state-lib state)
                 knowns? knownq
                 'match `(hash . ,fullhashes))))
        (if (nothing? rs)
            rs
            (stateful (list (car   details)
                            (cadr  details)
                            (caddr details)
                            (knowns-list rs))
                      state))))))
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
      (if (nothing? rs)
          rs
          (stateful '(#t)
                    (mk-state #f
                              #f
                              #f))))))

(define (fetch-profile)
  "Return a client mvalue which, when invoked, returns the result of a
viewq request."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lng state)        ; lounge connection
               views? viewq             ; predicate, constructor
               (state-tk state))))      ; input
      (if (nothing? rs)
          rs
          (stateful `(,(views-details rs))
                    (mk-state (views-token rs)
                              (state-lng   state)
                              (state-lib   state)))))))

(define (fetch-challenge-id)
  "Return chauths or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lng state)        ; lounge connection
               chauths? chauthq         ; predicate, constructor
               (state-tk state))))      ; input
      (if (nothing? rs)
          rs
          (stateful `(,(chauths-hash    rs)
                      ,(chauths-counter rs))
                    (mk-state (chauths-token rs)
                              (state-lng     state)
                              (state-lib     state)))))))

(define (fetch-challenge blobhash blobcounter)
  "Return challs or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               challs? challq           ; predicate, constructor
               blobhash blobcounter)))  ; inputs
      (if (nothing? rs)
          rs
          (stateful `(,(challs-challenge rs))
                    state)))))

(define (fetch-evaluation answer blobhash blobcounter)
  "Return evals or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               evals? evalq             ; predicate, constructor
               blobhash blobcounter
               answer)))                ; inputs
      (if (nothing? rs)
          rs
          (stateful `(,(evals-result   rs)
                      ,(evals-solution rs))
                    state)))))

(define (push-evaluation evaluation)
  "Return evals or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lng state)        ; lounge connection
               auths? evauthq           ; predicate, constructor
               (state-tk state)         ; token
               evaluation)))            ; input
      (if (nothing? rs)
          rs
          (stateful '(unimportant)
                    (mk-state (auths-token           rs)
                              (auths-prof-server     rs)
                              (auths-mod-server      rs)))))))

(define (fetch-hashmap crownsets)
  "Return hashmaps or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               hashmaps? hashmapq       ; predicate, constructor
               crownsets)))             ; input
      (if (nothing? rs)
          rs
          (stateful `(,(hashmaps-content rs))
                    state)))))

(define (fetch-hashpairs fullhashes)
  "Return a sethashess or ERROR."
  (lambda (state)
    (let ((rs (call/exchange
               (state-lib state)        ; library connection
               sethashess? sethashesq   ; predicate, constructor
               fullhashes)))            ; input
      (if (nothing? rs)
          rs
          (stateful `(,(sethashess-hashpairs rs))
                    (mk-state (state-tk  state)
                              (state-lng state)
                              (state-lib state)))))))

(define (push-scorecard hashmap)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (lambda (state)
    (let ((rs (apply push-data
                     'scorecard              ; thing to be updated,
                     hashmap                 ; input,
                     (state-lesser state)))) ; token, lounge
      (cond ((nothing? rs)
             rs)
            ((set!s? rs)
             (stateful `(,(set!s-value rs))
                       (mk-state (set!s-token rs)
                                 (state-lng   state)
                                 (state-lib   state))))
            (else
             (stateful '(unimportant)
                       (mk-state (auths-token       rs)
                                 (auths-prof-server rs)
                                 (auths-mod-server  rs))))))))

(define (push-active-modules hashpairs)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (lambda (state)
    (let* ((rs (apply push-data
                      'active-modules         ; thing to be updated,
                      hashpairs               ; input,
                      (state-lesser state)))) ; token, lounge
      (cond ((nothing? rs)
             rs)
            ((set!s? rs)
             (stateful `(,(set!s-value rs))
                       (mk-state (set!s-token rs)
                                 (state-lng state)
                                 (state-lib state))))
            (else
             (stateful 'unimportant
                       (mk-state (auths-token       rs)
                                 (auths-prof-server rs)
                                 (auths-mod-server  rs))))))))

;;;;; Helper Procedures
(define (push-data type data token profile-server)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (let ((result (lesser-call/exchange profile-server set!q
                                      token type data)))
    (if (or (auths? result)
            (set!s? result))
        result
        (nothing 'exchange-error result))))

(define (call/exchange target predicate rq . args)
  "Return the expected request of performing an RQ with ARGS on TARGET
as validated by PREDICATE. Raise an error if the response is not
expected."
  (let ((result (apply lesser-call/exchange target rq args)))
    (if (predicate result)
        result
        (nothing 'exchange-error result))))

(define (lesser-call/exchange target rq . args)
  "Return the request of performing an RQ with ARGS on TARGET. Raise
an error if the response is not expected."
  (let ((rs (exchange (request (apply rq args)) target)))
    (if rs
        (rs-content rs)
        (negs (apply rq args) '(exchange servers-down)))))
