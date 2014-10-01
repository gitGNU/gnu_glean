;;; glean --- fast learning tool.         -*- coding: utf-8 -*-

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

(define-module (tests server-responses)
  #:use-module (srfi srfi-1)      ; Provide fold
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (glean config)
  #:use-module (glean common comtools)
  #:use-module (glean common base-requests)
  #:use-module (glean common library-requests)
  #:use-module (quickcheck quickcheck)
  #:use-module (tests quickcheck-defs)
  #:use-module (tests test-utils))

(test-begin "server-tests")

;; Test a well behaving symbol message (it uses gwrite)
(test-assert "server-random-data"
  (unks? (rs-content (exchange 'random
                                 %library-port%))))

(quickname "random-data")
(quickcheck (lambda (_)
              (unks? (rs-content (exchange _
                                             %library-port%))))
            $symbol)

(test-assert "server-unknown-request"
             (unks?
              (rs-content (exchange (request 'random)
                                    %library-port%))))

(test-assert "server-alive"
             (acks?
              (rs-content (exchange (request (aliveq))
                                    %library-port%))))

;; (test-assert "get-profs"
;;   (fold (lambda (prof-list result)
;;           (if result
;;               (and (id? (cdr prof-list))
;;                    (string? (car prof-list)))
;;               #f))
;;         #t
;;         (profs-list (rs-content (exchange
;;                               (request (profs-rq)))))))

;; (test-assert "server-auth"
;;   (and (neg-rs? (rs-content (exchange
;;                              (request (auth-rq
;;                                           'random)))))
;;        (auth-rs?
;;         (rs-content
;;          (exchange
;;           (request (auth-rq (get-id tester-profile))))))))

;; test using bogus active-modules
(test-assert "server-#f-challenge"
             (negs?
              (rs-content
               (exchange
                (request (challq test-gprofile
                                 %lounge-port%))
                %library-port%))))

;; test using real module
(test-assert "server-challenge"
             (challs?
              (rs-content
               (exchange
                (request (challq "token"
                                 %lounge-port%))
                %library-port%))))

;; test using bogus active-modules
(test-assert "server-#f-eval"
             (and (negs?
                   (rs-content
                    (exchange
                     (request (evalq test-gprofile
                                     %lounge-port%
                                     "answer"))
                     %library-port%)))))

;; test using real module
(test-assert "server-eval"
             (evals?
              (rs-content
               (exchange
                (request (evalq "token"
                                %lounge-port%
                                "answer"))
                %library-port%))))

;; (test-assert "server-quit"
;;              (and (exchange (request (quit-rq)) %library-port%)
;;                   (begin (usleep 500)
;;                          (not (alive? %library-port%)))))

(test-end "server-tests")
