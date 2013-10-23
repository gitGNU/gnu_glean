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

(define-module (tests gmodule-manager)
  #:use-module (srfi srfi-1)       ; Provide map and reduce
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (tests test-utils)  ; Provide test-profiles, etc.
  #:use-module (guilecraft data-types sets) ; Provide gmod
					; introspection 

  #:use-module (guilecraft gmodule-manager)) ; interface to be tested

(test-begin "gmodule-manager")
;; Test the high level server interface for adding gmodules to
;; gmodule-provider backend (here, gmodule-manager), and for selecting
;; a gmodule from gmodule-provider.

(test-assert "Add gmodule to gmodule-manager"
	     (gman_add-gmodule test-gmodule))

;; Each entry in the active gmodule list returned by list-gmodules is
;; a pair consisting of a symbol (the gmodule's id) and a gmodule
;; object.
(test-assert "Retrieve list of loaded gmodules"
	     (and (symbol? (car (car (gman_list-gmodules))))
		  (set? (cdr (car (gman_list-gmodules))))))

;; select-gmodule is to return the gmodule object corresponding to the
;; gmodule ID supplied as parameter.
(test-assert "Return selected gmodule"
	     (set?
	      (gman_get-gmodule (set-id test-gmodule))))

(test-end "gmodule-manager")
