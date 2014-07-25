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

(define-module (tests gprofile-manager)
  #:use-module (srfi srfi-1)       ; Provide map and reduce
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (tests test-utils)  ; Provide test-profiles, etc.
  #:use-module (glean data-types gprofiles) ; Provide gprof
					; introspection 

  #:use-module (glean gprofile-manager)) ; interface to be tested

(test-begin "gprofile-selector")
;; Test the high level server interface for retrieving available gprofiles from
;; gprofile-provider backend (here, a dummy object), and for selecting
;; a gprofile from gprofile-provider.

(test-assert "Add gprofiles to the profile provider"
	     (and (add-gprofile test-gprofile)
		  (add-gprofile test-gprofile-2)))

;; list-gprofiles is to return a list consisting of gprofile names and
;; gprofile IDs for all gprofiles known to the glean server. It is
;; then down to the UI to provide means for the user to meaningfully
;; select from those gprofiles.
(test-assert "List known gprofiles"
	     (reduce (lambda (x y)
		       (and x y))
		     #f
		     (map (lambda (x)
			    (and (string? (car x))
				 (id? (cdr x))))
			  (list-gprofiles))))

;; select-gprofile is to return the gprofile object corresponding to the
;; gprofile ID supplied as parameter.
(test-assert "Return selected gprofile"
	     (profile?
	      (select-gprofile (profile-id test-gprofile-2))))

(test-assert "find-profile-by-name"
  (equal? (find-profile-by-name "test")
	  (select-gprofile (profile-id test-gprofile))))

(test-end "gprofile-selector")
