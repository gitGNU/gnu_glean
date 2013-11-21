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

;;; Commentary:
;;
;; gprofile-manager defines an interface to centrally store all known
;; gprofiles. The table is indexed on gprofile-id and it returns a
;; gprofile object.
;;
;; Code:

(define-module (guilecraft gprofile-manager)
  #:use-module (guilecraft data-manager)
  #:use-module (guilecraft data-types gprofiles)
  #:export  (add-gprofile
	     find-profile-by-name
	     select-gprofile
	     list-gprofiles))

; define a data-manager instance using the result of gprofile-id as
; key
(define gprofile-manager (data-manager profile-id))

(define (list-gprofiles)
  (map cons
       (map (lambda (x)
	      (profile-name (select-gprofile (car x))))
	    (gprofile-manager 'list))
       (map car
	    (gprofile-manager 'list))))

(define (find-profile-by-name name)
  (call/cc (lambda (k)
	     (begin
	       (map (lambda (pair)
		      (if (equal? name (profile-name (cdr pair)))
			  (k (cdr pair))))
		    (gprofile-manager 'list))
	       #f))))

(define (add-gprofile gprofile-object)
  "Convenience procedure to add a given gprofile to the
gprofile-manager.

gprofile-manager is an instance of data-manager. It stores the
gprofile-object indexed by its gprofile-id, derived using gprofile-id."
  (gprofile-manager 'put gprofile-object gprofile-object))

(define (select-gprofile gprofile-id)
  "Convenience procedure to retrieve a given gmodule from the active
gmodule-table stored in gmodule-manager."
  (gprofile-manager 'get gprofile-id))
