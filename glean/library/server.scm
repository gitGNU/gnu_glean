;; server.scm --- library server interface    -*- coding: utf-8 -*-
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
;; This server module implements the receiver of the libary server.  Properly,
;; its role is the initial parsing and handling of requests received.  To this
;; end I intend to implement a 'language as defense' approach, whereby this
;; module parses messages in their entirety, guaranteeing to the rest of the
;; library that the semantics of data passed to it is valid.
;;
;; At present this module falls short of its aims on two counts:
;; - it does not sufficiently parse incoming messages, and to the extent that
;;   it does parse, it does so haphazardly.
;; - it contains 'logic', or actions to do with library functionality. This
;;   functionality should take place in a separate module, to enhance
;;   separation of concerns, and to allow the re-use of library in a
;;   non-server context.
;;
;;; Code:

(define-module (glean library server)
  #:use-module (glean config)
  #:use-module (glean common base32)
  #:use-module (glean common base-server)
  #:use-module (glean common base-requests)
  #:use-module (glean common comtools)
  #:use-module (glean common hash)
  #:use-module (glean common library-requests)
  #:use-module (glean common lounge-requests)
  #:use-module (glean common utils)
  #:use-module (glean library lexp)
  #:use-module (glean library library-store)
  #:use-module (glean library sets)
  #:use-module (glean library set-operations)
  #:use-module (glean library set-tools)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs exceptions)
  #:export (library-server))


;;;;; Module Server Dispatch Logic
;;; Define the actual module server and the server-dispatcher used by it.

(define (library-server module-socket-file)
  (the-server module-socket-file server-dispatcher))

(define (server-dispatcher request)
  "Interprets client requests, and passes additional information for
handling to request handler."

  (cond ((eof-object? request)
         #f)

        ((request? request)
         (let ((rq (rq-content request)))
           (guard (err
                   (err
                    (exclaim "SERVER-DISPATCHER: error:~%  ~s.~%" err)
                    (negs rq err)))
             (cond ((aliveq? rq)
                    (acks rq))
                   ((challq? rq)
                    (challenge-provider rq))
                   ((evalq? rq)
                    (eval-provider rq))
                   ((knownq? rq)
                    (known-mods-provider rq))
                   ((detailq? rq)
                    (details-provider rq))
                   ((sethashesq? rq)
                    (sethashes-provider rq))
                   ((hashmapq? rq)
                    (hashmap-provider rq))
                   ((quitq? rq)
                    (acks rq))
                   (else (unks rq))))))

        (else (unks request))))

;;;;; Server Response Creation
;;; Functions that provide request specific parsing and response skeletons.

;;; <challq> -> <challs '((string. #f) (options) type)>
;;;             | (raise 'invalid-coordinates)>
(define (challenge-provider rq)
  "Return the <challs> belonging to the problem identified by the coordinates
in RQ, or raise 'invalid-coordinates."
  (define (new-challenge problem)
    "Return the challenge located in the set identified by BLOBHASH and
COUNTER, or raise 'invalid-set."
    ;; FIXME: this procedure is logic, not parsing, so it should be in
    ;; library-store, not in module-server.
    ;; FIXME: I've disabled passing back of media for now as they complicate
    ;; communication with clients - they need to be reworked anyway.
    (let ((s  (problem-s problem))
          (q  (problem-q problem))
          (os (problem-o problem)))
      (list (cons (q-text q) #f)        ; disabled media
            (if (null? os)
                '()
                (map (lambda (o) (cons (o-text o) #f)) os)) ; disabled media
            ;; FIXME: solution for allowing different question types.
            (cond ((not s)          'info)
                  ((and (list? s) (> (length s) 1)) 'multi)
                  ((null? os)       'open)
                  (else             'single)))))

  (match rq
    ;; XXX: PHANTOM is a non-existant field to make match work properly.
    (($ <challq> phantom (? list? lxp) (? hash? dag) (? hash? shallow)
        (? integer? counter))
     (match (fetch-problem lxp dag shallow counter)
       ((? upgrade-map? map) (upgs (serialize-upgrade-map map)))
       ((? problem? prob)    (challs (new-challenge prob)))))
    (_ (raise 'invalid-coordinates))))

;;; <evalq> -> <evals boolean (list | text)> | (raise 'invalid-coordinates)
(define (eval-provider rq)
  "Return the <evals> belonging to the problem identified by the coordinates
in RQ after evaluating it against the answer provided in RQ, or raise
'invalid-coordinates."
  (define (eval-answer problem answer)
    "Return the evaluation result of ANSWER with respect to PROBLEM."
    ;; FIXME: quick hack to allow for multiple solutions, or none.
    (match (fetch-solution problem)
      ("irrelevant" #t)                 ; no solution -> always right.
      ((? list? solutions)              ; multiple solutions.
       ;; XXX: seems broken to me.
       (fold (lambda (solution answer result)
               (if result
                   (equal? solution result)
                   #f))
             #t solutions answer))
      (solution (equal? solution answer))))
  (define (fetch-solution problem)
    (match (problem-s problem)
      ((? s? solution) (s-text solution))
      (((? s? solutions) ...) (s-text solution))
      (#f "irrelevant")))

  (match rq
    ;; XXX: PHANTOM is a non-existant field to make match work properly.
    (($ <evalq> phantom (? list? lxp) (? hash? dag) (? hash? shallow)
        (? integer? counter) (? string? answer))
     (match (fetch-problem lxp dag shallow counter)
       ;; We need an upgrade
       ((? upgrade-map? map) (upgs (serialize-upgrade-map map)))
       ;; We can evaluate the answer
       ((? problem? problem) (evals (eval-answer problem answer)
                                    (solution problem)))))
    (_ (raise 'invalid-coordinates))))

(define (known-mods-provider rq)
  (define (valid-search? pair)
    "Return #t if pair is a pair and contains a valid search pattern."
    (and (pair? pair)
         (cond ((eqv? (car pair) 'hash) ; hash
                (and (list? (cdr pair))
                     (null? (filter (negate symbol?)
                                    (cdr pair)))))
               ((eqv? (car pair) 'keywords) ; keywords
                (and (list? (cdr pair))
                     (null? (filter (negate string?)
                                    (cdr pair)))))
               ((eqv? (car pair) 'name) ; name
                (and (list? (cdr pair))
                     (null? (filter (negate string?)
                                    (cdr pair)))))
               (else                    ; fail
                #f))))
  (let ((operator (knownq-operator rq))
        (search   (knownq-search   rq)))
    (cond ((and (not operator)
                (not search))           ; known-crownsets
           (knowns (known-crownsets (catalogue-hash %current-catalogue%)
                                    %ignore-keywords%)))
          ;; search contains an invalid operator
          ((not (or (eqv? operator 'match))) ; operator
           (raise 'invalid-operator))
          ;; search contains only valid terms
          ((and (list? search)
                (valid-search? search)) ; search
           (knowns (search-sets operator search
                                (catalogue-hash %current-catalogue%))))
          (else                         ;fail
           (raise 'invalid-search)))))

(define (details-provider rq)
  (let ((hash (detailq-hash rq)))
    (if (hash? hash)
        (details (set-details hash (catalogue-hash %current-catalogue%)))
        (raise 'invalid-sethash))))

;; <hashmapq '((serialized-lxp . shallow-hash) ...)> -> <hashmaps hashmaps>
;; where hashmaps := '(hashmap ...)
(define (hashmap-provider rq)
  (define (fetch-sets hashes)
    (filter-map (lambda (hash)
                  (match (fetch-set hash (catalogue-hash %current-catalogue%))
                    ((('set . set) ('lexp . lxp)) set)
                    (_                            #f)))
                hashes))
  (match (hashmapq-hashpairs rq)
    ((((? (lambda (slxp) (lexp? (lexp-make slxp))) lxps)
       . (? hash? shallow-hashes)) ...)
     (match (map (cut make-hashmap <>) (fetch-sets shallow-hashes))
       (() (raise 'unknown-set-ids))
       (hashmps (hashmaps hashmps))))
    (else (raise 'invalid-set-ids))))

(define (sethashes-provider rq)
  (let ((fullhashes (sethashesq-fullhashes rq)))
    (if (and (list? fullhashes)
             (null? (filter (negate hash?) fullhashes)))
        (sethashess (set-hashpairs fullhashes
                                   (catalogue-hash %current-catalogue%)))
        (raise 'invalid-fullhashes))))

;;;; Procedures that feel like they should be in a different module

;;; serialized-lexp hash hash integer -> <problem> | <upgrade-map> | #f
(define (fetch-problem lxp dag shallow counter)
  "Return the problem associated with the coordinates LXP, DAG, SHALLOW and
COUNTER, or raise 'unknown-set."
  (match (fetch-set-from-lexp (lexp-make lxp)
                              (catalogue-hash %current-catalogue%))
    ((? plain-module? discipline)
     (if (string=? (dag-hash discipline) dag)
         ;; Should be good to go
         (match (fetch-set shallow (catalogue-hash %current-catalogue%))
           ((('set . set) ('lexp . lxp))
            (let ((problems (set-contents set)))
              (list-ref problems (modulo counter (length problems)))))
           (#f (throw 'glean-logic-error
                      "FETCH-PROBLEM: unexpected result.")))
         ;; Do we need an upgrade perhaps?
         (derive-upgrade-map discipline dag)))
    ;; DAG is invalid.
    (#f (throw 'glean-logic-error
               "FETCH-PROBLEM: unknown set."))))

;;; server.scm ends here
