;; monadic-min.scm --- monadic library to build clients  -*- coding: utf-8 -*-
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
;; Provide the common interfaces likely to be used by all other client
;; implementations. These are built using the 'client-monad', allowing us to
;; abstract from lounge context (tokens, lounge-connection and
;; library-connection).
;;
;;; Code:

(define-module (glean client monadic-min)
  #:use-module (glean config)
  #:use-module (glean common base-requests)
  #:use-module (glean common comtools)
  #:use-module (glean common library-requests)
  #:use-module (glean common lounge-requests)
  #:use-module (glean common monads)
  #:use-module (glean common utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (
            ;; state generating transactions
            register-player
            authenticate-player
            ;; composite transactions
            fallback-view-player
            view-player
            modify-player
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


;;;;; Client Monad
;;;
;;; A monad specialised for client stateful tasks. It provides state
;;; management, exception handling and logging.

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
  (let ((log (mlogger (lambda (obj)
                        (or (nothing? obj) (stateful? obj)))
                      client-monad-dict)))
    (lambda (st8)
      ;; Generate new-state-pair by passing state into mvalue.
      (let ((new-stateful (mvalue st8)))
        (log new-stateful (log-level))
        ;; Return the state-pair resulting from applying the new
        ;; state to mproc seeded with the new result.
        (if (nothing? new-stateful)
            new-stateful
            ((mproc (result new-stateful)) (state new-stateful)))))))

(define-monad client-monad
  (bind   client-bind)
  (return client-return))

(define (client-monad-dict object level)
  "Interpret OBJECT with regard to LEVEL, and return a list suitable for
interpretation by mlogger, to emit meaningful lounge-monad message."
  (let ((long? (eqv? level 'all)))
    (define (shorten obj)
      (if (symbol? obj)
          (string->symbol
           (string-append (string-take (symbol->string obj) 5) "..."))
          (string-append (string-take obj 5) "...")))
    (match object
      ((? stateful? sf)
       (match (result sf)
         (('test)                         ; test-servers
          `(test-servers "Status:" success))
         (('unimportant)                  ; push-{eval,prf,score} #t
          `(push-evaluation,profile,scorecard "Status:" success))
         ('unimportant                    ; push-actives #t
          `(push-active-modules "Status:" success))
         ((hash id name vers keyw syn desc auth res attr prop cont logo)
          `(fetch-detail "Set:" ,name))   ; fetch-detail
         (((name lng lib actives))        ; fetch-profile
          `(fetch-profile "Profile:" ,name))
         ((name lng lib actives)          ; det->full-det
          `(details->full-details "Full Profile:"
                                  ,(if long?
                                       (list name lng lib actives)
                                       name)))
         ((#t)                            ; push-deletion
          `(push-deletion "Result:" #t))
         ((? string? challenge)           ; fetch-challenge
          `(fetch-challenge "Challenge:" ,challenge))
         (((? boolean? res) solution)     ; fetch-evaluation
          `(fetch-evaluation "Result:" ,res))
         ((hash (? number? counter))      ; fetch-challenge-id #t
          `(fetch-challenge-id "Hash/Counter:"
                               ,(if long?
                                    (result sf)
                                    (list (shorten hash) counter))))
         ((set!s-field set!s-value)       ; fetch-challenge-id #f
          `(fetch-challenge-id "Update Required:"
                               ,(list set!s-field set!s-value)))
         ((((hash props) ...))            ; fetch-hashmap
          `(fetch-hashmap "Hashmap:"
                          ,(if long?
                               (result sf)
                               `(((,(shorten (car (car hash)))))))))
         ((((hash . fullhash)))           ; fetch-hashpairs
          `(fetch-hashpairs "Hash/Fullhash:"
                            ,(if long?
                                 (result sf)
                                 `(((,(shorten hash) .
                                     ,(shorten fullhash)))))))
         ((set!s-value)                   ; push-score,actives #f
          `(push-scorecard,active-modules "Value:" set!s-value))
         (res
          `(unknown "Stateful?:" ,res))))
      ((? nothing? noth)
       (let* ((id  (nothing-id noth))   ; Nothing msg
              (src (match id            ; Deduce src from id
                     ('library-down 'test-servers)
                     ('lounge-down  'test-servers)
                     (_ 'unknown))))
         `(,src "Nothing:" ,(if long?
                                (cons id (nothing-context noth))
                                id))))               ; -> log msg.
      (_ `(unknown "Result:" ,object)))))


;;;;; Communication Wrappers
;;;
;;; This section contains wrappers and convenience procedures for clients to
;;; communicate with glean servers.  The approach has been to provide a
;;; powerful general procedure (call/exchange), which is capable of returning
;;; very precise data sets from the server, and to then provide wrappers for
;;; individual requests and their responses that use call/exchange to provide
;;; data to the client program.

;;;; 'State' Generating Procedures

(define (register-player name password profile-server library-server)
  "Return 'lounge state' upon successful registration with the
lounge using NAME. Raise an Exchange Error otherwise."
  (let ((rs (call/exchange profile-server auths? regq
                           name password profile-server
                           library-server)))
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


;;;; Composite Transactions
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

(define (fallback-view-player state)
  "Return a module-less list of profile fields for display for the
profile identified by the token in STATE."
  ((mlet* client-monad
       ((test         (test-servers 'lounge))
        (details      (fetch-profile)))
     (return details)) state))

(define (modify-player id value state)
  "Return an auths confirming that the profile identified by STATE has
had its field (ID) updated with string VALUE."
  ((mlet* client-monad
       ((test      (test-servers 'lounge))
        (auths     (push-profile id value)))
     (return auths)) state))

(define* (add-active-modules fullhashes state #:optional (negate? #f))
  "Given a set of FULLHASHES provided, for instance, by the player
choosing from amongst a list of modules, carry out the necessary
transactions to activate these modules for the player."
  ((mlet* client-monad
       ((test       (test-servers))
        ;; Get set-hashpairs (minhash . fullhash)
        (hashpairs  (fetch-hashpairs fullhashes))
        ;; Update profile active modules, retrieve newly required hashmaps.
        (req-maps   (push-active-modules (if negate?
                                             (cons 'negate
                                                   (car hashpairs))
                                             (car hashpairs))))
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
  ((mlet* client-monad
       ((test              (test-servers))
        ;; Get next challenge blobhash/counter
        (challenge-details (fetch-challenge-id))
        (challenge         (match challenge-details
                             (('active-modules . _)
                              (return '(no-active-modules)))
                             (otherwise
                              (apply fetch-challenge otherwise))))
        (upgrade           (match challenge
                             ((('upgrade-map . _))
                              (push-upgrade (car challenge)))
                             (otherwise (return otherwise))))
        ;; Try again after upgrade
        (challenge-details (fetch-challenge-id))
        (challenge         (match challenge-details
                             (('active-modules . _)
                              (return '(no-active-modules)))
                             (otherwise
                              (apply fetch-challenge otherwise)))))
     (return challenge)) state))

(define (submit-answer answer state)
  "Given the usual STATE of token, lounge and library, submit the
player's answer for assesment, and push the result of assesment to the
player's profile."
  ((mlet* client-monad
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
  ((mlet* client-monad
       ((test (test-servers)))
     (push-deletion)) state))

(define (known-modules state)
  "Given the usual STATE of token, lounge and library, request library
provides us with details of available modules."
  ((mlet* client-monad
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
          (else (stateful '(test)
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
  "Return a client-monad mval for a delpq request."
  (lambda (state)
    "Return a steteful whose result's first values is #t upon
successful deletion of the profile identified by the lounge server and
the token in STATE. Raise an Exchange Error otherwise."
    (let ((rs (call/exchange
               (state-lng state)        ; lounge connection
               acks? delpq              ; predicate, constructor
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
    (match (call/exchange (state-lng state) ; lounge connection
                          (lambda (rs)      ; predicate
                            (or (chauths? rs)
                                (set!s? rs)))
                          chauthq           ; constructor
                          (state-tk state))
      ((? nothing? rs) rs)
      ((? set!s? rs)
       (stateful `(,(set!s-field rs) ,(set!s-value rs))
                 (mk-state (set!s-token   rs)
                           (state-lng     state)
                           (state-lib     state))))
      ((? chauths? rs)
       (stateful `(,(chauths-lexp rs) ,(chauths-dag-hash rs)
                   ,(chauths-shallow-hash rs) ,(chauths-counter rs))
                 (mk-state (chauths-token rs)
                           (state-lng     state)
                           (state-lib     state)))))))

(define (fetch-challenge lexp dag shallow counter)
  "Return challs or ERROR."
  (lambda (state)
    (match (call/exchange (state-lib state) ; library connection
                          (lambda (rs)      ; predicate
                            (or (challs? rs)
                                (upgs? rs)))
                          challq            ; constructor
                          lexp dag shallow counter)
      ((? nothing? rs) rs)
      ;; XXX: PHANTOM is a non-existant field to make match work properly.
      (($ <upgs> phantom content)
       (stateful `(,content) state))
      ((? challs?  rs) (stateful `(,(challs-challenge rs)) state)))))

(define (fetch-evaluation answer lexp dag shallow counter)
  "Return evals or ERROR."
  (lambda (state)
    (match (call/exchange (state-lib state) ; library connection
                          (lambda (rs)      ; predicate
                            (or (evals? rs)
                                (upgs? rs)))
                          evalq             ; constructor
                          lexp dag shallow counter answer)
      ((? nothing? rs) rs)
      ;; XXX: PHANTOM is a non-existant field to make match work properly.
      (($ <upgs> phantom content)
       (stateful `(,content) state))
      ((? evals?  rs) (stateful `(,(evals-result   rs)
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

(define (push-profile id data)
  "Return a client mvalue built with ID and DATA, which when invoked
attempts to perform a profile update, returning an auths confirming
success or a nothing value."
  (lambda (state)
    (let* ((rs (apply push-data id data (state-lesser state))))
      (cond ((nothing? rs)
             rs)
            (else
             (stateful 'unimport
                       (mk-state (auths-token       rs)
                                 (auths-prof-server rs)
                                 (auths-mod-server  rs))))))))

(define (push-upgrade map)
  (lambda (state)
    (match (apply push-data 'upgrade map (state-lesser state))
      ((? nothing? rs) rs)
      ((? auths?   rs) (stateful 'unimportant
                                 (mk-state (auths-token       rs)
                                           (auths-prof-server rs)
                                           (auths-mod-server  rs))))
      (otherwise (throw 'glean-logic-error
                        "PUSH-UPGRADE: unexpected response:" otherwise)))))


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

;;; monadic-min.scm ends here
