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
              50 $mk-rootset))
(test-assert "basic set generation"
  (quickcheck (lambda (_) (set? _))
              50 $mk-set))

(test-assert "set generation"
  (quickcheck (lambda (_) (set? _))
              50 $set))
(test-assert "module generation"
  (quickcheck (lambda (_) (plain-module? _))
              50 $module))
(test-assert "tutorial generation"
  (quickcheck (lambda (_) (tutorial? _))
              50 $tutorial))

(test-assert "rootset num problems"
  (quickcheck (lambda (_)
                (= 5 (number-of-problems _)))
              50 (lambda () ($mk-rootset 5))))

(test-assert "set num problems"
  (quickcheck (lambda (_)
                (= (* 5 5) (number-of-problems _)))
              50 (lambda () ($mk-set 5))))

(test-end "sets")
