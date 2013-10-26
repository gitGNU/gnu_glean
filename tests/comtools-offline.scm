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
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (quickcheck quickcheck)
  #:use-module (tests quickcheck-defs)
  #:use-module (rnrs)
  #:use-module (guilecraft comtools)) ; Provide functions to be
				      ; tested.

(test-begin "transformation-tests")

(quickname "robustness-tests")
(quickname "record->list")
(quickcheck (lambda (_) (list? (record->list _))) $record)
(quickname "list->record")
(quickcheck (lambda (_) (record? (list->record _))) $tagged-list)
(quickname "record->list*")
(quickcheck (lambda (_) (list? (record->list* _))) $record)
(quickname "list->record*")
(quickcheck (lambda (_) (record? (list->record* _))) $tagged-list)

;; Dummy objects for procedures used by "challenge request" and "eval
;; request".
(define test-scorecard
  (make-scorecard
   (list
    (make-mod-blob 'git (list
			  (make-set-blob 'status 0 0)
			  (make-set-blob 'branch 50 5)))
    (make-mod-blob 'bzr (list
			  (make-set-blob 'st 0 3)
			  (make-set-blob 'commit 50 7))))))

(test-assert "record->list*"
	     (equal?
	      (record->list* test-scorecard)
	      '(":symbol: scorecard"
		((":symbol: mod-blob" ":symbol: git"
		  ((":symbol: set-blob" ":symbol: status" 0 0)
		   (":symbol: set-blob" ":symbol: branch" 50 5)))
		 (":symbol: mod-blob" ":symbol: bzr"
		  ((":symbol: set-blob" ":symbol: st" 0 3)
		   (":symbol: set-blob" ":symbol: commit" 50 7)))))))

(test-assert "list->record*"
	     (equal? (list->record* (record->list*
				     test-scorecard))
		     test-scorecard))

(test-end "transformation-tests")
