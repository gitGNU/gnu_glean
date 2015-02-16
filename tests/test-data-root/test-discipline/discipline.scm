;; discipline.scm --- the test-discipline discipline -*- coding: utf-8 -*-
;; This file is part of Glean.
;;
;; Copyright (C) YEAR AUTHOR <EMAIL>
;; Created: DAY MONTH YEAR
;;
;; Glean is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; Glean is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with glean; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; This module defines the test-discipline discipline.
;;
;;; Code:

(define-module
  (glean disciplines test-discipline discipline)
  #:use-module
  (glean library core-templates)
  #:export
  (test-discipline))

(define test-discipline
  (module
      'test-discipline
      #:name "A unit test fixture."
      #:version "0.1"
      #:keywords '("education" "code" "tests")
      #:synopsis "Merely used for unit tests."
      #:description "Really, that's it. Just unit tests."
      #:creator "Alex Sassmannshausen"
      #:contents `()))

;;; discipline.scm ends here
