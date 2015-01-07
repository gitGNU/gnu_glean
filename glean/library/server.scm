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
  #:use-module (glean common library-requests)
  #:use-module (glean common lounge-requests)
  #:use-module (glean common utils)
  #:use-module (glean library sets)
  #:use-module (glean library library-store)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs exceptions)
  #:export (library-server))


;;;;; Blobhash?
;;; This should not be needed here.  There should be a library side module
;;; which provides this procedure.  After all, we know the great secret which
;;; is that a blobhash is the same as a sethash.

(define (blobhash? obj)
  "Return #t if OBJ is a blobhash, #f otherwise."
  (symbol? obj))

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

(define (challenge-provider rq)
  (define (new-challenge problem)
    "Return the challenge located in the set identified by BLOBHASH and
COUNTER, or raise 'invalid-set."
    ;; FIXME: this procedure is logic, not parsing, so it should be in
    ;; library-store, not in module-server.
    ;; FIXME: I've disabled passing back of media for now — they need to be
    ;; reworked anyway.
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

  (let ((bh (challq-hash rq))
        (c (challq-counter rq)))
    (cond ((not (blobhash? bh))
           (raise 'invalid-blobhash))
          ((not (number? c))
           (raise 'invalid-counter))
          (else (challs (new-challenge (fetch-problem bh c)))))))

(define (fetch-problem blobhash counter)
  (let ((set (fetch-set blobhash (catalogue-hash %current-catalogue%))))
    (if set
        (let* ((problems (set-contents set))
               (num-of-problems (length problems)))
          (list-ref problems (modulo counter num-of-problems)))
        (raise 'unknown-set))))

(define (eval-provider rq)
  (define (eval-answer problem answer)
    "Return the evaluation result of ANSWER with respect to PROBLEM."
    ;; FIXME: quick hack to allow for multiple solutions, or none.
    (let ((solution (problem-s problem)))
      (cond ((not solution) #t)
            ((list? solution)
             (let ((sol (map s-text solution)))
               (fold (lambda (solution answer result)
                       (if result
                           (equal? solution result)
                           #f))
                     #t sol answer)))
            (else
             (equal? (s-text solution) answer)))))
  (define (fetch-solution problem)
    (if (not (problem-s problem))
        "irrelevant"
        (s-text (problem-s problem))))

  (let ((bh (evalq-hash rq))
        (c (evalq-counter rq))
        (answer (evalq-answer rq)))
    (cond ((not (blobhash? bh))
           (raise 'invalid-blobhash))
          ((not (number? c))
           (raise 'invalid-counter))
          ((not (or (string? answer) (list? answer)))
           (raise 'invalid-answer))
          (else
           (let ((problem (fetch-problem bh c)))
             (evals (eval-answer problem answer)
                    (fetch-solution problem)))))))

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
    (if (blobhash? hash)
        (details (set-details hash (catalogue-hash %current-catalogue%)))
        (raise 'invalid-sethash))))

(define (hashmap-provider rq)
  (let ((hashpairs (hashmapq-hashpairs rq)))
    (if (and (list? hashpairs)
             (fold (lambda (hashpair previous)
                     (and previous (pair? hashpair)
                          (blobhash? (car hashpair))   ; minhash
                          (blobhash? (cdr hashpair)))) ; fullhash
                   #t hashpairs))
        (let ((sets (filter-map
                     (lambda (hashpair)
                       (fetch-set (cdr hashpair)
                                  (catalogue-hash %current-catalogue%)))
                     hashpairs)))
          (if (not (null? sets))
              (hashmaps (map make-hashtree sets))
              (raise 'unknown-set-ids)))
        (raise 'invalid-set-ids))))

(define (sethashes-provider rq)
  (let ((fullhashes (sethashesq-fullhashes rq)))
    (if (and (list? fullhashes)
             (null? (filter (negate blobhash?) fullhashes)))
        (sethashess (set-hashpairs fullhashes
                                   (catalogue-hash %current-catalogue%)))
        (raise 'invalid-fullhashes))))

;;; server.scm ends here
