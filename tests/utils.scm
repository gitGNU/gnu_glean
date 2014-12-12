;; utils.scm --- tests for utils    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 15 November 2014
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
;; Unit tests for utils.
;;
;; Source-file: glean/common/utils.scm
;;
;;; Code:

(define-module (tests utils)
  #:use-module (glean common utils)
  #:use-module (srfi srfi-64)
  )


;;;; Tests

(test-begin "utils")
;;; Test whether `make-logger' respects logging instructions.
(test-assert "logger-off"
  (not ((make-logger #f #f #f) 'message)))
(test-equal "logger-current-output"
  "Test Message"
  (with-output-to-string
    (lambda ()
      ((make-logger #t #f #f) 'exclaim '("Test Message")))))
(test-end)

;;; utils ends here
