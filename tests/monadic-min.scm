;; monadic-min.scm --- tests for monadic-min    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 13 November 2014
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
;; Unit tests for monadic-min.
;;
;; Source-file: glean/client/monadic-min.scm
;;
;;; Code:

(define-module (tests monadic-min)
  #:use-module (glean common base-requests)
  #:use-module (glean common monads)
  #:use-module (glean common utils)
  #:use-module (glean client monadic-min)
  #:use-module (glean config)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (tests quickcheck-defs))


;;;; Tests

(test-begin "monadic-min")

(let ((proc (@@ (glean client monadic-min) client-monad-dict)))
  ;; test basic monad-dict functionality:
  ;; - test query
  ;; - shortening of stateful
  ;; - shortening of nothing
  ;; - long stateful
  ;; - long nothing
  (parameterize ((log-level 'exclaim))
    (test-equal "test"
      '(test-servers "Status:" success)
      (proc (stateful '(test) 'state) (log-level)))
    (test-equal "hash/fullhash-short"
      '(fetch-hashpairs "Hash/Fullhash:" ((("54321..." . "12345..."))))
      (proc (stateful '((("543210000" . "123450000"))) 'state) (log-level)))
    (test-equal "nothing-short"
      '(unknown "Nothing:" nothing-short)
      (proc (nothing 'nothing-short '(irrelevant)) (log-level))))
  (parameterize ((log-level 'all))
    (test-equal "hash/fullhash-long"
      '(fetch-hashpairs "Hash/Fullhash:" ((("543210000" . "123450000"))))
      (proc (stateful '((("543210000" . "123450000"))) 'state) (log-level)))
    (test-equal "nothing-long"
      '(unknown "Nothing:" (nothing-long . (irrelevant)))
      (proc (nothing 'nothing-long '(irrelevant)) (log-level)))))

;;;; Exchange tests

(when (or (not (file-exists? %default-lounge%))
          (not (file-exists? %default-library%)))
  (format #t "Lounge and/or Library not online: skip stateful tests.~%")
  (test-skip 100))

(define test-name "test-name")
(define test-pw   "")
(define test-creds `(,test-name ,test-pw ,%default-lounge%))

;;;;; Tests for: register-player

(test-assert "register-player"
  ;; FIXME: using $string instead of "proper name" causes failure.
  (match (register-player test-name test-pw %default-lounge% %default-library%)
    ((? state? st8)
     (and (string=? %default-lounge% (state-lng st8))
          (string=? %default-library% (state-lib st8))))))

(test-assert "register-player-taken"
  (match (register-player test-name test-pw %default-lounge% %default-library%)
    ((? nothing? noth)
     (eq? 'username-taken (neg-msg (nothing-context noth))))))

;;;;; Tests for: authenticate-player

(test-assert "authenticate-player"
  (match (apply authenticate-player test-creds)
    ((? state? st8)
     (and (string=? %default-lounge% (state-lng st8))
          (string=? %default-library% (state-lib st8))))))

(test-assert "authenticate-player-unknown"
  (and (match (authenticate-player "unknown-name" "test" %default-lounge%)
         ((? nothing? noth)
          (eq? 'unknown-user (neg-msg (nothing-context noth)))))
       (match (authenticate-player test-name "test" %default-lounge%)
         ((? nothing? noth)
          (eq? 'unknown-user (neg-msg (nothing-context noth)))))))

(define* (auth #:optional (format 'records))
  (apply authenticate-player (append test-creds `(,format))))

(define (sxml-test obj predicate)
  (match obj
    (('stateful ('result (? predicate))
                (or ('state ('token   tk)
                            ('lounge  lng)
                            ('library lib)
                            ('format  format))
                    ('state #f)))
     #t)))

;;;;; Tests for: view-player

(define (view-player-result? result)
  (match result ((test-name %default-lounge% %default-library% ()) #t)))

(test-assert "view-player"
  (match (view-player (auth))
    ((? stateful? st8f) (view-player-result? (result st8f)))))

(test-assert "view-player-sxml"
  (sxml-test (view-player (auth 'sxml)) view-player-result?))

;;;;; Tests for: fallback-view-player

(define (fallback-view-player-result? result)
  (match result ((test-name %default-lounge% %default-library% ()) #t)))

(test-assert "fallback-view-player"
  (match (fallback-view-player (auth))
    ((? stateful? st8f) (fallback-view-player-result? (result st8f)))))

(test-assert "fallback-view-player-sxml"
  (sxml-test (fallback-view-player (auth 'sxml))
             fallback-view-player-result?))

;;;;; Tests for: modify-player

(define (modify-player-result? result) (and (boolean? result) result))

(test-assert "modify-player-name"
  (let* ((test-name2 "new-name")
         (auth2 (lambda ()
                  (apply authenticate-player test-name2 (cdr test-creds)))))
    (and
     (sxml-test (modify-player 'name `(,test-name2 . ,test-pw) (auth 'sxml))
                modify-player-result?)
     (match (modify-player 'name `(,test-name . ,test-pw) (auth2))
       ((? stateful? st8f) (modify-player-result? (result st8f)))))))

(test-assert "modify-player-password"
  (let* ((test-pw2 "new-pass")
         (auth2 (lambda ()
                  (apply authenticate-player (car test-creds) test-pw2
                         (cddr test-creds)))))
    (and (sxml-test (modify-player 'password test-pw2 (auth 'sxml))
                    modify-player-result?)
         (match (modify-player 'password test-pw (auth2))
           ((? stateful? st8f) (match (result st8f) (#t #t)))))))

(test-assert "modify-player-library"
  (and (match (modify-player 'mod-server "test" (auth))
         ((? stateful? st8f)
          (and (match (result st8f) (#t #t))
               (match (state st8f)
                 ((? state? st8)
                  (and (string=? %default-lounge% (state-lng st8))
                       (string=? "test" (state-lib st8))))))))
       (match (modify-player 'mod-server %default-library% (auth))
         ((? stateful? st8f)
          (and (match (result st8f) (#t #t))
               (match (state st8f)
                 ((? state? st8)
                  (and (string=? %default-lounge% (state-lng st8))
                       (string=? %default-library% (state-lib st8))))))))))

(test-assert "modify-player-lounge"
  (and (match (modify-player 'prof-server "test" (auth))
         ((? stateful? st8f)
          (and (match (result st8f) (#t #t))
               (match (state st8f)
                 ((? state? st8)
                  (and (string=? "test" (state-lng st8))
                       (string=? %default-library% (state-lib st8))))))))
       (let ((st8 (auth)))
         (match (modify-player 'prof-server %default-lounge%
                               (mk-state (state-tk st8) %default-lounge%
                                         (state-lib st8)))
           ((? stateful? st8f)
            (and (match (result st8f) (#t #t))
                 (match (state st8f)
                   ((? state? st8)
                    (and (string=? %default-lounge% (state-lng st8))
                         (string=? %default-library% (state-lib st8)))))))))))

;;;;; Tests for: known-modules

(define (known-modules-result? result)
  (match result
    (() (error "No disciplines in the library."))
    (((hashes ids names versions keywords synopsi logos) ...) hashes)
    (_ (error "Wrong man... Just wrong."))))

(test-assert "known-modules"
  (match (known-modules (auth))
    ((? stateful? st8f) (known-modules-result? (result st8f)))
    (_ (error "Definitely wrong."))))

(test-assert "known-modules-sxml"
  (sxml-test (known-modules (auth 'sxml)) known-modules-result?))

(define (knowns)
  (match (result (known-modules (auth)))
    (((hashes ids names versions keywords synopsi logos) ...) hashes)
    (_ #f)))

;;;;; Tests for: view-set

;; We test all disciplines in the store.  If this fails, it could be a faulty
;; discipline, or faulty view-set.

(define (view-set-result? result)
  (match result
    ((hash id name version keywords synopsis description creator
           'resources 'attributes properties children logo) #t)))

(test-assert "view-set"
  (match (map (cute view-set <> (auth)) (knowns))
    (((? stateful? st8fs) ...)
     (fold (lambda (current previous)
             (if previous (view-set-result? current) #f))
           #t (map result st8fs)))))

(test-assert "view-set-sxml"
  (sxml-test (view-set (car (knowns)) (auth 'sxml)) view-set-result?))

;;;;; Tests for: add-active-modules

;; We test adding all disciplines in the store.  If this fails, it could be a
;; faulty discipline or a faulty add-active-modules.
(test-assert "add-active-modules"
  (match (add-active-modules (knowns) (auth))
    ((? stateful? st8f) (modify-player-result? (result st8f)))))

;; The sensible next step would be to de-activate the disciplines (or even to
;; purge them).  Neither is currently working.  This results in us having to
;; test either 'records or 'sxml based add-active-modules.

(test-skip 1)
(test-assert "add-active-modules-sxml"
  (sxml-test (add-active-modules (knowns) (auth 'sxml))
             modify-player-result?))

;;;;; Tests for: next-challenge

(define (next-challenge-result? result)
  (match result ((((problem . resources) options type)) #t)))

;; FIXME: output of next-challenge is incompatible with sxml. Reasons:
;; Return of unlabeled list, use of pair.
;; Also the output is needlessly wrapped in a list.
(test-assert "next-challenge"
  (match (next-challenge (auth))
    ((? stateful? st8f) (next-challenge-result? (result st8f)))))

(test-assert "next-challenge-sxml"
  (sxml-test (next-challenge (auth 'sxml)) next-challenge-result?))

;;;;; Tests for: submit-answer

(define (submit-answer-result? result)
  (match result (((? boolean?) solution) #t)))

(test-assert "submit-answer"
  (match (submit-answer "bogus" (auth))
    ((? stateful? st8f) (submit-answer-result? (result st8f)))))

(test-assert "submit-answer-sxml"
  (sxml-test (submit-answer "bogus" (auth 'sxml)) submit-answer-result?))

;;;;; Tests for: delete-player

(test-assert "delete-player"
  (match (delete-player (auth))
    ((? stateful? st8f) (modify-player-result? (result st8f)))))

(test-assert "delete-player-sxml"
  (begin
    (register-player test-name test-pw %default-lounge% %default-library%)
    (sxml-test (delete-player (auth 'sxml)) modify-player-result?)))

(test-end "monadic-min")

;;; monadic-min ends here
