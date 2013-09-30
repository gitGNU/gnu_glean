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
;; Module to provide defining procedures to load init data: modules
;; and profiles.
;;
;;; Code:

(define-module (guilecraft store)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (guilecraft gmodule-manager)
  #:use-module (guilecraft gprofile-manager)
  #:export (store-modules
	    store-profiles))

(define (data-dir postfix)
  (string-append (dirname (dirname (search-path %load-path "guilecraft/store.scm")))
                 postfix))

(define %module-directory
  ;; Absolute path of the guilecraft modules root.
  (data-dir "/modules"))
(define %profile-directory
  ;; Absolute path of the guilecraft profiles root.
  (data-dir "/profiles"))

(define (store-modules)
  (map (module-loader-generator gman_add-gmodule)
       (data-modules %module-directory)))

(define (store-profiles)
  (map (module-loader-generator add-gprofile)
       (data-modules %profile-directory)))

(define (module-loader-generator proc)
  (define (data-loader-generator proc)
    (lambda (name value)
      (catch #t
	(lambda () (proc (variable-ref value)))
	(lambda (key . args)
	  (simple-format #t "failed to load ~a" (symbol->string name))
	  (newline)))))

  (lambda (module)
    (module-map (data-loader-generator proc) 
		module)))

(define (data-files directory)
  "Return the list of files that implement distro modules."
  (define prefix-len
    (string-length
     (dirname directory)))

  (file-system-fold (const #t)                    ; enter?
                    (lambda (path stat result)    ; leaf
                      (if (string-suffix? ".scm" path)
                          (cons (substring path prefix-len) result)
                          result))
                    (lambda (path stat result)    ; down
                      result)
                    (lambda (path stat result)    ; up
                      result)
                    (const #f)                    ; skip
                    (lambda (path stat errno result)
                      (error (_ "cannot access `~a': ~a~%")
                               path (strerror errno))
                      result)
                    '()
		    directory
                    stat))

(define (data-modules directory)
  "Return the list of modules that provide packages for the distribution."
  (define not-slash
    (char-set-complement (char-set #\/)))

  (filter-map (lambda (path)
                (let ((name (map string->symbol
                                 (string-tokenize (string-drop-right path 4)
                                                  not-slash))))
                  (false-if-exception (resolve-interface name))))
              (data-files directory)))
