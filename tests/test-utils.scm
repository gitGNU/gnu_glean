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

(define-module (tests test-utils)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft problem-types open-problems)
  #:use-module (guilecraft problem-types multi-choice-problems)

  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft gprofile-ops)

  #:export (test-gmodule
	    test-gprofile
	    test-gprofile-2))

(define test-gmodule
  (gmodule
   (id 'test)
   (name "Test Gmodule")
   (version "0.1")
   (description "Test Description")
   (long-description "Long Description:")
   (creators "Alex Sassmannshausen")
   (derivation-source "None")
   (parts
    (list
     (gset_make-gset
      'gset-tag
      (list (make-open-problem "question?"
			       "solution")
	    (make-multi-choice-problem "question?"
				       (cons "b" "option b")
				       (cons "a" "option a")
				       (cons "b" "option b")
				       (cons "c" "option c"))))))
   (find-out-more "http://some.url")))

(define test-gprofile
  (make-profile (name "test")
		      (id (make-id "test" 1366787517))
		      (active-modules '(test))
		      (scorecard (make-scorecard '()))))

(define test-gprofile-2
  (make-profile (name "test2")
		      (id (make-id "test2" 1366787517))
		      (active-modules '(test2))
		      (scorecard (make-scorecard '()))))
