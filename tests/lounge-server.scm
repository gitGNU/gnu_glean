;; lounge-server.scm --- lounge-server unit tests    -*- coding: utf-8 -*-
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
;; Lounge-server unit tests.
;;
;; Source-file: glean/lounge/server.scm
;;
;;; Code:

(define-module (tests lounge-server)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (glean config)
  #:use-module (glean common base-requests)
  #:use-module (glean common lounge-requests)
  #:use-module (glean common monads)
  #:use-module (glean lounge lounge-store)
  #:use-module (glean lounge server)
  #:use-module (ice-9 vlist))

(define (server-dispatcher rq)
  ((@@  (glean lounge server) server-dispatcher) rq))
(define %name     "Blogs")
(define %pass     "J0e")
(define %lng      "lng")
(define %lib      "lib")
(define %npass    "J03")
(define %nname    "Alice")
(define %nlng     "new-lng")
(define %nlib     "new-lib")
(define %act-mods '((git . etrdurined)))
(define %sccard   '(((etrdurined . ((module . #t)))
                     (((child1 . ()))               ; subtree 1
                      ((child2 . ((tutorial . #t))) ; subtree 2
                       (((child21 . ()))            ; subtree 1
                        ((child22 . ()))))))))      ; subtree 2

(test-begin "lounge-server")

(test-assert "Registration"
             (let ((auths (server-dispatcher (request
                                              (regq %name %pass
                                                    %lng %lib)))))
               (and (auths? auths)
                    (string=? (auths-prof-server auths) %lng)
                    (string=? (auths-mod-server  auths) %lib))))
(test-assert "Duplicate Registration"
             (let ((negs (server-dispatcher (request
                                             (regq %name "passw0rd"
                                                   %lng %lib)))))
               (and (negs? negs)
                    (eqv? (neg-msg negs) 'username-taken))))
(test-assert "Authenticate"
             (let ((auths (server-dispatcher (request
                                              (authq %name %pass)))))
               (and (auths? auths)
                    (string=? (auths-prof-server auths) %lng)
                    (string=? (auths-mod-server  auths) %lib))))
(test-assert "Set Lounge"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name %pass)))))
                    (rt (server-dispatcher (request
                                            (set!q tk
                                                   'prof-server
                                                   %nlng))))
                    (rs (server-dispatcher (request
                                            (set!q (auths-token rt)
                                                   'mod-server
                                                   %nlib)))))
               (and (auths? rs)
                    (string=? (auths-prof-server rt) %nlng)
                    (string=? (auths-mod-server  rs) %nlib))))
(test-assert "Premature Fetch Challenge"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name %pass)))))
                    (rs (server-dispatcher (request (chauthq tk)))))
               (and (set!s? rs)
                    (eqv? (set!s-field rs) 'active-modules))))
(test-assert "Set Active Mod"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name %pass)))))
                    (rs (server-dispatcher (request
                                            (set!q tk 'active-modules
                                                   %act-mods)))))
               (and (set!s? rs)
                    (eqv? (set!s-field rs) 'scorecard)
                    (equal? (set!s-value rs) %act-mods))))
(test-assert "View Profile"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name %pass)))))
                    (rs (server-dispatcher (request (viewq tk)))))
               (and (views? rs)
                    (equal? (views-details rs)
                            (list %name %nlng %nlib
                                  %act-mods)))))
(test-assert "Update Scorecard"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name %pass)))))
                    (rs (server-dispatcher
                         (request (set!q tk 'scorecard %sccard)))))
               (and (auths? rs)
                    (string=? (auths-prof-server rs) %nlng)
                    (string=? (auths-mod-server  rs) %nlib))))
(test-assert "Set Password"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name %pass)))))
                    (rt (server-dispatcher
                         (request (set!q tk 'password %npass)))))
               (and (auths? rt)
                    (views? (server-dispatcher
                             (request (viewq (auths-token rt)))))
                    (auths? (server-dispatcher
                             (request (authq %name %npass)))))))
(test-assert "Set Name"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name %npass)))))
                    (rt (server-dispatcher
                         (request (set!q tk 'name (cons %nname %npass))))))
               (and (auths? rt)
                    (views? (server-dispatcher
                             (request (viewq (auths-token rt)))))
                    (auths? (server-dispatcher
                             (request (authq %nname %npass)))))))
(test-assert "Fetch Challenge"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %nname %npass)))))
                    (rs (server-dispatcher (request (chauthq tk)))))
               (and (chauths? rs)
                    (eqv? (chauths-hash rs) 'child1)
                    (= (chauths-counter rs) 0))))
(test-assert "Eval Challenge"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %nname %npass)))))
                    (rs (server-dispatcher (request (evauthq tk
                                                             #t)))))
               (and (auths? rs)
                    (string=? (auths-prof-server rs) %nlng)
                    (string=? (auths-mod-server  rs) %nlib))))
(test-assert "Delete Registration"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %nname %npass)))))
                    (delps (server-dispatcher (request (delpq tk)))))
               (and (acks? delps)
                    (delpq? (ack-orig delps)))))

(test-end "lounge-server")

;;; discipline ends here
