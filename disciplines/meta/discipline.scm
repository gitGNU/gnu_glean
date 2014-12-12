;; discipline.scm --- Example of a meta discipline    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 06 June 2014
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
;; A discipline aiming to show inter-discipline dependence and linking.
;;
;;; Code:

(define-module (glean disciplines meta discipline)
  #:use-module (glean library core-templates)
  #:use-module (glean store git)
  #:use-module (glean store test)
  #:export (meta-module))

(define meta-module
  (module
    'meta
    #:name "Meta: Git and Test combined"
    #:version "0.1"
    #:keywords '("test" "recursion")
    #:synopsis "Learn to use meta"
    #:description "Long Description: background on git, introductory text"
    #:creator "Alex Sassmannshausen"
    #:attribution (list (media #:text "Git man pages & website"))
    #:contents `(,git-module ,test-module)
    #:resources (list (media #:urls '("http://www.git-scm.com")))))

;;; discipline ends here
