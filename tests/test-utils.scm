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

(define-module (tests test-utils)
  #:use-module (glean config)
  #:use-module (glean library sets)
  #:use-module (glean lounge profiles)
  #:use-module (glean lounge scorecards)

  #:export (test-gmodule
            test-gprofile
            test-gprofile-2))

(define test-gmodule
  (module
    'test
    #:name "Test Gmodule"
    #:version "0.1"
    #:synopsis "Test Description"
    #:description "Long Description:"
    #:creator "Alex Sassmannshausen"
    #:attribution (list (media #:urls '("None")))
    #:contents (list
                (set
                 'gset-tag
                 #:contents (list
                             (problem
                              (q "question?")
                              (s "solution"))
                             (problem
                              (q "question?")
                              (s "option b")
                              (o "option a")
                              (o "option b")
                              (o "option c")))))
    #:resources (list (media #:urls '("http://some.url")))))

(define test-gprofile
  (make-bare-profile "test"
                     %lounge-port%
                     %library-port%))

(define test-gprofile-2
  (make-profile  "test2"
                 %lounge-port%
                 %library-port%
                 '(test)
                 (make-scorecard '())))
