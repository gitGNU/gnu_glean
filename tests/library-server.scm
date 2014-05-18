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

(define-module (tests library-server)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft data-types module-requests)
  #:use-module (guilecraft library-store)
  #:use-module (guilecraft module-server))

(define (server-dispatcher rq)
  ((@@  (guilecraft module-server) server-dispatcher) rq))

(test-begin "library-server")

(test-assert "Known Crownsets"
             (and (knowns? (server-dispatcher (request (knownq))))))
(test-assert "Set Detail"
             ;; Should also test for rootset results.
             (let* ((hash (car (car (knowns-list
                                     (server-dispatcher
                                      (request (knownq)))))))
                    (rs   (server-dispatcher
                           (request (detailq hash)))))
               (and (details? rs)
                    (list?    (details-list rs)))))

(test-end "library-server")
