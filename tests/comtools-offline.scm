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

(define-module (tests comtools-offline)
  #:use-module (srfi srfi-64)		; Provide test suite
  #:use-module (tests test-utils)	; Provide test-profiles, etc.
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft known-rtd-manager)
  #:use-module (guilecraft comtools)) ; Provide functions to be
				      ; tested.

(test-begin "transformation-tests")

;; Dummy objects for procedures used by "challenge request" and "eval
;; request".
(define test-scorecard
  (make-scorecard
   (list
    (make-gmod-blob 'git (list
			  (make-gset-blob 'status 0 0)
			  (make-gset-blob 'branch 50 5)))
    (make-gmod-blob 'bzr (list
			  (make-gset-blob 'st 0 3)
			  (make-gset-blob 'commit 50 7)))
    test-gmodule
    test-gprofile
    test-gprofile-2)))
(define rtd
  (record-type-descriptor test-scorecard))
(define rtn
  (record-type-name rtd))

(test-assert "known-rtds-put"
	     (known-rtds 'put rtn rtd))

(test-assert "known-rtds-get"
	     (and (eq? (known-rtds 'get '<scorecard>) rtd)))

(test-assert "record->list*"
	     (equal? (record->list* test-scorecard)
		     '(<scorecard> 
		       ((<score-gmod-blob> 
			 git 
			 ((<score-gset-blob> status 0)
			  (<score-gset-blob> branch 50)))
			(<score-gmod-blob>
			 bzr
			 ((<score-gset-blob> st 0)
			  (<score-gset-blob> commit 50)))
			(<gmodule>
			 test
			 "Test Gmodule"
			 "0.1"
			 "Test Description"
			 "Long Description:"
			 "Alex Sassmannshausen"
			 ((<gset>
			   gset-tag
			   ((<open-problem> "question?" "solution")
			    (<multi-choice-problem>
			     "question?"
			     ("b" . "option b")
			     (("a" . "option a")
			      ("b" . "option b")
			      ("c" . "option c"))))))
			 "http://some.url" "None")
			(<profile> "test"
				   (<id> "test" 1366787517)
				   (test) (<scorecard> ()))
			(<profile> "test2"
				   (<id> "test2" 1366787517)
				   (test2) (<scorecard> ()))))))

(test-assert "list->record*"
	     (scorecard? (list->record* (record->list*
					 test-scorecard))))

(test-assert "full-known-rtds"
	     (and (known-rtds 'get '<scorecard>)
		  (known-rtds 'get '<gset>)
		  (known-rtds 'get '<profile>)
		  (known-rtds 'get '<gmodule>)
		  (known-rtds 'get '<score-gset-blob>)
		  (known-rtds 'get '<score-gmod-blob>)
		  (known-rtds 'get '<id>)))

(test-assert "not-random-known-rtd"
	     (not (known-rtds 'get '<blah>)))

(test-end "transformation-tests")
