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

;;;; Commentary:
;;;
;;; Provide a command line client for Guilecraft.
;;;
;;;; Code:

(define-module (guilecraft tools)
  #:use-module (guilecraft config)
  #:export (install-module
	    edit-module
	    retrieve-module))

(define (install-module file-name)
  (catch #t
    (lambda ()
      (let ((wip-module (string-append %wip-module-directory% "/"
				       file-name))
	    (module (string-append %module-directory% "/"
				   file-name)))
	(load wip-module)
	(simple-format #t "~a: successfully parsed." file-name)
	(newline)
	(simple-format #t "~a: now installing." file-name)
	(newline)
	(rename-file wip-module module)
	(simple-format #t "~a: installed." file-name)
	(newline)))
    (lambda (key . args)
      (simple-format #t "error: ~a ~a" key args)
      (newline)
      (simple-format #t "Failed to checkin module.")
      (newline))))

(define (retrieve-module file-name)
  (catch #t
    (lambda ()
      (let ((wip-module (string-append %wip-module-directory% "/"
				       file-name))
	    (module (string-append %module-directory% "/"
				   file-name)))
	(simple-format #t "~a: checking out." file-name)
	(newline)
	(rename-file module wip-module)
	(simple-format #t "~a: checked out." file-name)
	(newline)))
    (lambda (key . args)
      (simple-format #t "error: ~a ~a" key args)
      (newline)
      (simple-format #t "Failed to checkout module.")
      (newline))))

(define (edit-module file-name)
  (retrieve-module file-name)
  (system (string-append "$EDITOR "
			 %wip-module-directory%
			 "/"
			 file-name)))
