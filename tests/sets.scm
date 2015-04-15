;; sets.scm --- set unit tests    -*- coding: utf-8 -*-
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
;; Set unit tests.
;;
;;; Code:

(define-module (tests sets)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (quickcheck quickcheck)
  #:use-module (tests quickcheck-defs)
  #:use-module (glean library core-templates)
  #:use-module (glean library sets))

(define ($module)
  (module 
    ($symbol)
    #:contents (($short-list $prob))
    #:name ($string) #:version ($string)
    #:synopsis ($string) #:description ($string)
    #:keywords (($short-list $string))
    #:creator ($string)
    #:attribution (($short-list $medii))
    #:resources (($short-list $medii))
    #:logo ($string)
    #:properties (($short-assoc $symbol $string))))
(define ($set)
  (set
    ($symbol)
    #:contents (($short-list $prob))
    #:name ($string) #:version ($string)
    #:synopsis ($string) #:description ($string)
    #:keywords (($short-list $string))
    #:creator ($string)
    #:attribution (($short-list $medii))
    #:resources (($short-list $medii))
    #:logo ($string)
    #:properties (($short-assoc $symbol $string))))
(define ($tutorial)
  (tutorial
    ($symbol)
    #:contents (($short-list $prob))
    #:name ($string) #:version ($string)
    #:synopsis ($string) #:description ($string)
    #:keywords (($short-list $string))
    #:creator ($string)
    #:attribution (($short-list $medii))
    #:resources (($short-list $medii))
    #:logo ($string)
    #:properties (($short-assoc $symbol $string))))

(test-begin "sets")
;; Test basic set generation
(test-assert "basic rootset generation"
  (quickcheck (lambda (_) (set? _))
              10 $mk-rootset))
(test-assert "basic set generation"
  (quickcheck (lambda (_) (set? _))
              10 $mk-set))

(test-assert "set generation"
  (quickcheck (lambda (_) (set? _))
              10 $set))
(test-assert "module generation"
  (quickcheck (lambda (_) (plain-module? _))
              10 $module))
(test-assert "tutorial generation"
  (quickcheck (lambda (_) (tutorial? _))
              10 $tutorial))

(test-assert "rootset num problems"
  (quickcheck (lambda (_)
                (= 5 (number-of-problems _)))
              10 (lambda () ($mk-rootset #:problems 5))))

(test-assert "set num problems"
  (quickcheck (lambda (_)
                (= (* 5 5) (number-of-problems _)))
              10 (lambda () ($mk-set #:children 5))))

(test-end "sets")

;;; discipline ends here
