;; test-utils.scm --- test utilities    -*- coding: utf-8 -*-
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
;; Utilities for other tests.
;;
;;; Code:

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

;;; discipline ends here
