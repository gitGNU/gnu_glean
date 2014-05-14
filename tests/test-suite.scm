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
;; Centralised way to run all tests.
;;
;;; Code:

(define-module (tests test-suite)
  #:use-module (guilecraft config)
  #:export (run-test-suite)
  #:export (run-server-tests))

(define (run-test-suite)
  (with-cwd (string-append %guilecraft-dir% "/logs/")
	    (lambda ()
	      (use-modules
               (tests base32)
               (tests hash)
               ;; FIXME: Old style scorecards
               ;;(tests scorecards)
               ;; FIXME: Uses make-mod-blob
               ;; (tests comtools-offline)
               (tests lounge-server)
               )
	      (let* ((path %library-port%))
		(if (and (access? path W_OK)
			 (catch #t
			   (lambda ()
			     (let ((s (socket PF_UNIX SOCK_STREAM 0)))
			       (connect s AF_UNIX path)
			       (write "alive?" s)
			       (read s)
			       (close s)))
			   (lambda (k . args) #f)))
		    (use-modules (tests comtools-online)
                                 ;; FIXME: Causes Crash
				 ;;(tests server-responses)
                                 ;;DEPRECATED:
                                 ;;(tests clients-min)
                                 )
		    (begin (format #t "
No server is running; we will skip communication tests.\n")
			   (newline)))))))

(define (with-cwd path thunk)
  (let ((old-path (getcwd)))
    (if (access? path F_OK)
	(chdir path)
	(begin
	  (mkdir path)
	  (chdir path)))
    (thunk)
    (chdir old-path)))
