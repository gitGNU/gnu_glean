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

(define-module (tests profiler)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft profiler))

(test-begin "profiler-tests")

(test-equal "lowest-scoring-gset-good"
	    (make-set-blob 'tag
			    3
			    1)
	    (lowest-scoring-gset
	     (make-mod-blob 'gmodule-id
			     (list (make-set-blob 'tag-false
						   7
						   1)
				   (make-set-blob 'tag-wrong
						   5
						   1)
				   (make-set-blob 'tag
						   3
						   1)
				   (make-set-blob 'false
						   8
						   6)))))

(test-assert "lowest-scoring-gset-ugly"
	     (catch #t
	       (lambda ()
		 (lowest-scoring-gset
		  (make-mod-blob 'gmodule-id
				  '())))
	       (lambda (key . args)
		 key args ; ignored
		 #t)))

(test-equal "profiler-good"
	    (make-mod-blob 'gmodule-true
			    (list (make-set-blob 'true
						  3
						  1)))
	    (profiler
	     (make-profile
	      "test"
	      (make-id 'test 123)
	      '(gmodule-wrong gmodule-true)
	      (make-scorecard
	       (list (make-mod-blob 'gmodule-wrong
				    (list (make-set-blob 'tag-false
							 7
							 1)))
		     (make-mod-blob 'gmodule-true
				    (list (make-set-blob 'true
							 3
							 1)))
		     (make-mod-blob 'gmodule-red-herring
				    (list (make-set-blob 'false
							 1
							 1))))))))
(test-assert "profiler-bad"
	     (not
	      (profiler
	       (make-profile
		"test"
		(make-id 'test 123)
		'()
		(make-scorecard
		 '(we never get to this))))))

(test-end "profiler-tests")
