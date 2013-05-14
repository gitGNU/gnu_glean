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

(define-module (tests scorecards)
  #:use-module (srfi srfi-64)
  #:use-module (tests test-utils)

  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft scorecard-ops))

(test-begin "scorecard-tests")

(test-assert "dummy-score-gset-blob"
	     (and (score-gset-blob? (make-dummy-score-gset-blob))
		  (dummy-score-gset-blob? (make-dummy-score-gset-blob))))

(test-assert "dummy-score-gmod-blob"
	     (and (score-gmod-blob? (make-dummy-score-gmod-blob))
		  (dummy-score-gmod-blob? (make-dummy-score-gmod-blob))))

(test-assert "empty-scorecard" (empty-scorecard? (make-scorecard '())))

(test-equal "first in scorecard" 
	    (make-score-gmod-blob 
	     'test (list (make-score-gset-blob 'gset-tag 0)))
	    (first-in-scorecard (make-scorecard-skeleton 
				 (list test-gmodule-object))))

(test-equal "rest of scorecard"
	    '()
	    (rest-of-scorecard
	     (make-scorecard-skeleton (list test-gmodule-object))))

(test-assert "skeleton-scorecard" 
	     (scorecard? (make-scorecard-skeleton 
			  (list test-gmodule-object))))

(test-assert "lower score" 
	     (lower-score? (make-score-gset-blob 'foo 1)
			   (make-score-gset-blob 'bar 4)))

(test-eq "update-scorecard" 
	 1
	 (score-gset-blob-score 
	  (car (score-gmod-blob-data
		(first-in-scorecard
		 (update-scorecard (make-scorecard-skeleton (list test-gmodule-object))
				   'test
				   'gset-tag
				   #t))))))

(test-end "scorecard-tests")

