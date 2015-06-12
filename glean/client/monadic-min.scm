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
;; FIXME: Currently, the only way to test this is to have a lounge and library
;; running.  This isn't great.  One way around this would be to have:
;; a) comtools (specifically `exchange') work with generic ports.
;; b) have well-defined exchange language (sxml), to generate messages for
;;    mock objects.
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
  (lambda (st8)
    "Return a state-pair of value and ST8."
    (match (state-format st8)
      ('records (stateful value st8))
      ('sxml    `(stateful (result ,value)
                           ,(state-serialize st8 (state-format st8)))))))

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
        (match new-stateful
          ;; Error in process
          ((? nothing?)
           (match (state-format st8)
             ('records new-stateful)
             ('sxml    `(nothing
                         (nothing-id ,(nothing-id new-stateful))
                         (nothing-context ,(nothing-context new-stateful))))))
          ;; Early return
          (('stateful ('result value) state)
           `(stateful (result ,value)
                      ,(state-serialize st8 (state-format st8))))
          ;; Continue
          (_ ((mproc (result new-stateful)) (state new-stateful))))))))

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
         (#t                              ; push-deletion
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

(define* (register-player name password profile-server library-server
                          #:optional (format 'records))
  "Return 'lounge state' upon successful registration with the
lounge using NAME. Raise an Exchange Error otherwise."
  (let ((rs (call/exchange profile-server auths? regq
                           name password profile-server
                           library-server)))
    (if (nothing? rs)
        rs
        (mk-state (auths-token rs) (auths-prof-server rs)
                  (auths-mod-server rs) format))))

(define* (authenticate-player name password profile-server
                              #:optional (format 'records))
  "Return 'lounge state' upon successful authentication with the
lounge using NAME. Raise an Exchange Error otherwise."
  (let ((rs (call/exchange profile-server auths? authq
                           name password)))
    (if (nothing? rs)
        rs
        (mk-state (auths-token rs) (auths-prof-server rs)
                  (auths-mod-server rs) format))))


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
        (full-details (details->full-details details)))
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
                                             (cons 'negate hashpairs)
                                             hashpairs)))
        ;; Get hashmaps if necessary.  If not, Return valid mvalue.
        (hashmap    (match req-maps
                      (#t (return #t))
                      (_  (fetch-hashmap req-maps))))
        ;; Update profile scorecards with hashmaps if necessary.
        (auths      (push-scorecard hashmap)))
     (return auths)) state))

(define (next-challenge state)
  "Given the usual STATE of token, lounge and library, return the
next-challenge for the player associated with token."
  ((mlet* client-monad
       ((test              (test-servers))
        ;; Get next challenge blobhash/counter
        (challenge-details (fetch-challenge-id))
        (challenge         (match challenge-details
                             (('active-modules _)
                              (lambda (st8) (nothing 'no-active-modules #t)))
                             ((lxp dhash shash counter)
                              (fetch-challenge lxp dhash shash counter))))
        ;; Check for upgrade, if we have active modules.
        (upgrade           (match challenge
                             ((('upgrade-map . _))
                              (push-upgrade (car challenge)))
                             (_ (return challenge))))
        ;; Try again after upgrade, if necessary.
        (new-details       (fetch-challenge-id))
        (new-challenge     (apply fetch-challenge challenge-details)))
     (return new-challenge)) state))

(define (submit-answer answer state)
  "Given the usual STATE of token, lounge and library, submit the
player's answer for assesment, and push the result of assesment to the
player's profile."
  ((mlet* client-monad
       ((test              (test-servers))
        (challenge-details (fetch-challenge-id))
        (evaluation        (apply fetch-evaluation answer challenge-details))
        ;; Update profile scorecard…
        (pushed            (push-evaluation (car evaluation))))
     ;; …But return evaluation result + new state
     (return evaluation)) state))

(define (delete-player state)
  "Given the usual STATE of token, lounge and library, request lounge
delete the player identified by token."
  ((mlet* client-monad
       ((test (test-servers))
        (ok   (push-deletion)))
     (return ok)) state))

(define (known-modules state)
  "Given the usual STATE of token, lounge and library, request library
provides us with details of available modules."
  ((mlet* client-monad
       ((test   (test-servers 'library))
        (knowns (fetch-known-modules)))
     (return knowns)) state))


;;;;; Helper Procedures
;;;
;;; We should integrate below in our new macros

(define (call/exchange target predicate rq . args)
  "Return the expected request of performing an RQ with ARGS on TARGET
as validated by PREDICATE. Raise an error if the response is not
expected."
  (let ((rq (apply rq args)))
    (match (exchange (request rq) target)
      ((? response? rs)
       (match (rs-content rs)
         ((? predicate result) result)
         (otherwise            (nothing 'exchange-error otherwise))))
      (otherwise        (nothing 'exchange-error
                                 (negs rq '(exchange servers-down)))))))

(define* (exchange-do #:key (predicate (const #f)) (constructor (const #f))
                      (value-extractors '()) (ins '()) lounge?
                      state-extractor)
  "Helper procedure, avoiding complex repetition in xchange macro."
  (define (augment-ins st8) (if lounge? (cons (state-tk st8) ins) ins))
  (define (proc-map obj procs) (map (lambda (proc) (proc obj)) procs))
  (define targetter (if lounge? state-lng state-lib))

  (lambda (st8)
    (match (apply call/exchange (targetter st8) predicate constructor
                  (augment-ins st8))
      ((? nothing? nothing) nothing)
      (rs (stateful (match value-extractors
                      (()          #t)
                      ((extractor) (extractor rs))
                      (extractors  (proc-map rs extractors)))
                    ;; If this is a lounge operation then we expect a means
                    ;; to extract a new state from the response and state.
                    (if state-extractor (state-extractor rs st8) st8))))))

;;;;; Helper Macros

(define-syntax xchange
  ;; Wrapper providing lounge/library communication convenience.
  (syntax-rules (input: apply: library: lounge: token)
    ((_ (library: predicate constructor)
        (input: ins ...)
        (apply: extractor* ...))
     (exchange-do #:predicate        predicate
                  #:constructor      constructor
                  #:value-extractors `(,extractor* ...)
                  #:ins              `(,ins ...)))

    ((_ (lounge: predicate constructor st8-extractor)
        (input: ins ...)
        (apply: extractor* ...))
     (exchange-do #:predicate        predicate
                  #:constructor      constructor
                  #:value-extractors `(,extractor* ...)
                  #:ins              `(,ins ...)
                  #:lounge?          #t
                  #:state-extractor  st8-extractor))))

(define-syntax push-xchange
  ;; Wrapper providing lounge profile updating convenience.
  (syntax-rules (input:)
    ((_ (input: field value))
     (xchange (lounge: (lambda (rs) (or (auths? rs) (set!s? rs)))
                       set!q
                       (lambda (rs st8)
                         (match rs
                           ((? set!s?) (set-state-tk st8 (set!s-token rs)))
                           ((? auths?) (mk-state (auths-token       rs)
                                                 (auths-prof-server rs)
                                                 (auths-mod-server  rs)
                                                 (state-format      st8))))))
              (input:  field value)
              (apply:  (lambda (rs)
                         (match rs
                           ((? set!s?) `(,(set!s-value rs)))
                           ((? auths?) #t))))))))


;;;; Atomic Transactions / Monadic Transactions
;;;
;;; We must here introduce a way to return proper sxml return values
;;; (without breaking backward compatibility), including for errors, so we can
;;; ease the creation of non-Guile clients (e.g. Emacs UI).

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


;;;;; Library Requests

(define (fetch-challenge lexp dag shallow counter)
  "Return challs or ERROR."
  (xchange (library: (lambda (rs) (or (challs? rs) (upgs? rs))) challq)
           (input:   lexp dag shallow counter)
           (apply:   (lambda (rs)
                       (match rs
                         (($ <upgs> phantom content) `(,content))
                         ((? challs?) `(,(challs-challenge rs))))))))

(define (fetch-evaluation answer lexp dag shallow counter)
  "Return evals or ERROR."
  (xchange (library: (lambda (rs) (or (evals? rs) (upgs? rs))) evalq)
           (input:   lexp dag shallow counter answer)
           (apply:   (lambda (rs)
                       (match rs
                         (($ <upgs> phantom content) `(,content))
                         ((? evals?)
                          (map (cute <> rs)
                               `(,evals-result ,evals-solution))))))))

(define (fetch-hashmap crownsets)
  "Return hashmaps or ERROR."
  (xchange (library: hashmaps? hashmapq)
           (input:   (car crownsets))
           (apply:   hashmaps-content)))

(define (fetch-hashpairs shallowhashes)
  "Return a sethashess or ERROR."
  (xchange (library: sethashess? sethashesq)
           (input:   shallowhashes)
           (apply:   sethashess-hashpairs)))

(define (fetch-known-modules)
  "Return a client-monad mval for an availq."
  (xchange (library: knowns? knownq)
           (input:   #f #f)
           (apply:   knowns-list)))

(define (fetch-detail shallowhash)
  "Return a client-monad mval which, when invoked, returns details
about the set identified by SHALLOWHASH or nothing."
  (xchange (library: details? detailq)
           (input:   shallowhash)
           (apply:   details-list)))

(define (details->full-details details)
  "Return a client-monad mval, resolving to new DETAILS with
human-friendly information about active-modules."
  (match details
    ((name lounge library ((lxps . hashes) ...))
     (xchange (library: knowns? knownq)
              (input:   'match `(hash . ,hashes))
              (apply:   (lambda (rs)
                          `(,name ,lounge ,library ,(knowns-list rs))))))))


;;;;; Lounge Requests

(define (fetch-profile)
  "Return a client mvalue which, when invoked, returns the result of a
viewq request."
  (xchange (lounge: views? viewq (lambda (rs st8)
                                   (set-state-tk st8 (views-token rs))))
           (input:  )
           (apply:  views-details)))

(define (fetch-challenge-id)
  "Return chauths or ERROR."
  (xchange (lounge: (lambda (rs) (or (chauths? rs) (set!s? rs)))
                    chauthq
                    (lambda (rs st8)
                      (set-state-tk st8
                                    (match rs
                                      ((? set!s?)   (set!s-token rs))
                                      ((? chauths?) (chauths-token rs))))))
           (input:  )
           (apply:  (lambda (rs)
                      (match rs
                        ((? set!s?)
                         (map (cute <> rs) `(,set!s-field ,set!s-value)))
                        ((? chauths?)
                         (map (cute <> rs)
                              (list chauths-lexp chauths-dag-hash
                                    chauths-shallow-hash chauths-counter))))))))

;;;;; Simple Lounge Updates

(define (push-evaluation evaluation)
  "Return evals or ERROR."
  (xchange (lounge: auths? evauthq (lambda (rs st8)
                                     (mk-state (auths-token       rs)
                                               (auths-prof-server rs)
                                               (auths-mod-server  rs)
                                               (state-format      st8))))
           (input:  evaluation)
           (apply:  )))

(define (push-deletion)
  "Return a client-monad mval for a delpq request."
  (xchange (lounge: acks? delpq (lambda (rs st8)
                                  (mk-state #f #f #f (state-format st8))))
           (input:  )
           (apply:  )))

;;;;; Specialized Lounge Updates

(define (push-scorecard hashmap)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (push-xchange (input: 'scorecard hashmap)))

(define (push-active-modules hashpairs)
  "Return an auths confirming success, a set!s requesting further data
or ERROR."
  (push-xchange (input: 'active-modules hashpairs)))

(define (push-profile id data)
  "Return a client mvalue built with ID and DATA, which when invoked
attempts to perform a profile update, returning an auths confirming
success or a nothing value."
  (push-xchange (input: id data)))

(define (push-upgrade map)
  (push-xchange (input: 'upgrade map)))

;;; monadic-min.scm ends here
