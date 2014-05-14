;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

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

(define-module (tests lounge-server)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (guilecraft config)
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft data-types profile-requests)
  #:use-module (guilecraft profile-server))

(define (server-dispatcher rq)
  ((@@  (guilecraft profile-server) server-dispatcher) rq))
(define %name     "Blogs")
(define %lng      "lng")
(define %lib      "lib")
(define %nlng     "new-lng")
(define %nlib     "new-lib")
(define %act-mods '((git . etrdurined)))
(define %sccard   '((etrdurined ((child1)
                                 (child2 ((child21)
                                          (child22)))))))

(test-begin "lounge-server")

(test-assert "Registration"
             (let ((auths (server-dispatcher (request (regq %name
                                                     %lng
                                                     %lib)))))
               (and (auths? auths)
                    (string=? (auths-prof-server auths) %lng)
                    (string=? (auths-mod-server  auths) %lib))))
(test-assert "Duplicate Registration"
             (let ((negs (server-dispatcher (request (regq %name
                                                    %lng
                                                    %lib)))))
               (and (negs? negs)
                    (eqv? (neg-msg negs) 'username-taken))))
(test-assert "Authenticate"
             (let ((auths (server-dispatcher (request
                                              (authq %name)))))
               (and (auths? auths)
                    (string=? (auths-prof-server auths) %lng)
                    (string=? (auths-mod-server  auths) %lib))))
(test-assert "Set Lounge"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name)))))
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
(test-assert "Set Active Mod"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name)))))
                    (rs (server-dispatcher (request
                                            (set!q tk 'active-modules
                                                   %act-mods)))))
               (and (set!s? rs)
                    (eqv? (set!s-field rs) 'scorecard)
                    (equal? (set!s-value rs) %act-mods))))
(test-assert "Update Scorecard"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name)))))
                    (rs (server-dispatcher
                         (request (set!q tk 'scorecard %sccard)))))
               (and (auths? rs)
                    (string=? (auths-prof-server rs) %nlng)
                    (string=? (auths-mod-server  rs) %nlib))))
(test-assert "Fetch Challenge"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name)))))
                    (rs (server-dispatcher (request (chauthq tk)))))
               (and (chauths? rs)
                    (eqv? (chauths-hash rs) 'child1)
                    (= (chauths-counter rs) 0))))
(test-assert "Eval Challenge"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name)))))
                    (rs (server-dispatcher (request (evauthq tk
                                                             #t)))))
               (and (auths? rs)
                    (string=? (auths-prof-server rs) %nlng)
                    (string=? (auths-mod-server  rs) %nlib))))
(test-assert "Delete Registration"
             (let* ((tk (auths-token (server-dispatcher
                                      (request (authq %name)))))
                    (dels (server-dispatcher (request (delq tk)))))
               (and (acks? dels)
                    (delq? (ack-orig dels)))))

(test-end "lounge-server")
