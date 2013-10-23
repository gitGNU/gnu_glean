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
  #:use-module (quickcheck quickcheck)
  #:use-module (tests quickcheck-defs)

  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft scorecard-ops))

(test-begin "scorecard-tests")

(quickname "scorecard-skeleton")
(quickcheck (lambda (x)
	      (scorecard? (make-scorecard-skeleton x)))
	    ($short-list $set))

(test-assert "dummy-set-blob"
  (and (set-blob? (make-dummy-set-blob))
       (dummy-set-blob? (make-dummy-set-blob))))

(test-assert "dummy-mod-blob"
  (and (mod-blob? (make-dummy-mod-blob))
       (dummy-mod-blob? (make-dummy-mod-blob))))

(test-assert "The empty scorecard"
  (null-scorecard? (make-empty-scorecard)))

(test-equal "first in scorecard"
  (make-mod-blob
   'test (list (make-set-blob 'gset-tag 0 0)))
  (car-mod-blobs (scorecard-data (make-scorecard-skeleton
				   (list test-gmodule)))))

(test-equal "rest of scorecard"
  '()
  (cdr-mod-blobs (scorecard-data (make-scorecard-skeleton
				   (list test-gmodule)))))

(test-assert "lower score"
  (lower-score? (make-set-blob 'foo 1 3)
		(make-set-blob 'bar 4 1)))

(test-eq "update-scorecard"
	 1
	 (set-blob-score
	  (car-set-blobs
	   (mod-blob-data
	    (car-mod-blobs
	     (scorecard-data
	      (update-scorecard (make-scorecard-skeleton (list test-gmodule))
				'test
				'gset-tag
				#t)))))))

(test-end "scorecard-tests")
