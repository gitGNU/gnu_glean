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

(define-module (scorecard-tests)
  #:use-module (srfi srfi-64)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft problem-types open-problems)
  #:use-module (guilecraft problem-types multi-choice-problems)
  #:use-module (guilecraft data-types gmodules)

  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft scorecard-ops))

(define test-gmodule-object 
  (gmod_make-gmodule
   (id 'test)
   (name "Test Gmodule")
   (version "0.1")
   (description "Test Description")
   (long-description "Long Description:")
   (creators "Alex Sassmannshausen")
   (derivation-source "None")
   (parts (list (gset_make-gset 'gset-tag
				(list (make-open-problem "question?"
							 "solution")
				      (make-multi-choice-problem "question?"
								 (cons "b" "option b")
								 (cons "a" "option a")
								 (cons "b" "option b")
								 (cons "c" "option c"))))))
   (find-out-more "http://some.url")))

(test-begin "scorecard-tests")

(test-assert "dummy-datum" (and (scorecard-datum? (make-dummy-scorecard-datum))
			      (dummy-scorecard-datum? (make-dummy-scorecard-datum))))

(test-assert "dummy-subdatum" (and (scorecard-subdatum? (make-dummy-scorecard-subdatum))
				 (dummy-scorecard-subdatum? (make-dummy-scorecard-subdatum))))

(test-assert "empty-scorecard" (empty-scorecard? (make-scorecard '())))

(test-equal "first in scorecard" 
  (make-scorecard-datum 'test
			(list (make-scorecard-subdatum 'gset-tag
						       0)))
  (first-in-scorecard (make-scorecard-skeleton (list test-gmodule-object))))

(test-equal "rest of scorecard"
  '()
  (rest-of-scorecard (make-scorecard-skeleton (list test-gmodule-object))))

(test-assert "skeleton-scorecard" (scorecard? (make-scorecard-skeleton (list test-gmodule-object))))

(test-assert "lower score" (lower-score? (make-scorecard-subdatum 'foo
								  1)
					 (make-scorecard-subdatum 'bar
								  4)))

(test-end "scorecard-tests")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
