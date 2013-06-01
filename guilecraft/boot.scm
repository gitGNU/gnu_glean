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
;; Module to parse options, etc before dropping into the main loop.
;;
;;; Code:

(define-module (guilecraft boot)    
  #:use-module (ice-9 format)      ; Print output
  #:use-module (ice-9 getopt-long) ; Manipulate command-line options
  #:use-module (srfi srfi-19)      ; To store and manipulate time
			           ; effectively
  #:use-module (tests test-suite)  ; In case of -t option: run
				   ; test-suite
  #:export (boot))

;; Define the list of accepted options and their special properties
(define *option-grammar* '((listen)
                           (usage (single-char #\u))
			   (config (value #t) (single-char #\c))
			   (test-suite (single-char #\t))
                           (version (single-char #\v))
                           (help (single-char #\h))))

(define usage 
  (lambda ()
    "Dispatch a usage message, with permitted command-line options, to gdisplay for output."
    (define repr-option 
      (lambda (opt)
	"Return, as string the car of OPT.
Options will be surrounded by square brackets if optional."
	(string-append "[--" (object->string (car opt)) "]")))
    (format #t (string-append "usage: guilecraft "
			      (string-join (map repr-option *option-grammar*))
			      "\n"))
    (format #t "For now you should run guilecraft with the --listen flag, or one of the other flags — else guilecraft will return a read prompt and exit.\n")))
  
  

(define version (lambda ()
		  (begin
		    (display "Guilecraft version 0.1")
		    (newline))))

;; krap code
(define parse-options (lambda (args)
  (let ((opts (getopt-long args *option-grammar*)))
    (if (or (option-ref opts 'usage #f)
            (option-ref opts 'help #f)
            (not (null? (option-ref (cdr opts) '() '()))))
        (begin
          (usage)
          (exit 0)))
    (if (option-ref opts 'test-suite #f)
	(run-test-suite))
    (if (option-ref opts 'version #f)
        (begin
          (version)
          (exit 0)))
    (if (option-ref opts 'listen #f)
        ((@ (system repl server) spawn-server)))
    opts)))

(define (boot args)
  "Set the locale, parse the options, drop into the main loop."
  (setlocale LC_ALL "") ; sets the locale to the system locale
  (let ((options (parse-options args))
	(start-clock (current-time)))
    (let ((config (option-ref options 'config #f)))
      (if config
          (let ((config-module (resolve-module '(guilecraft config))))
            (save-module-excursion
             (lambda ()
               (set-current-module config-module)
               (primitive-load config))))))
    (main-loop)))

;;; Just a place-holder
(define (main-loop)
  "The main loop which calls challenges and expects answers, until the kill signal."
  (begin ;(gdisplay (controller 'generate-question))
	 ;(gdisplay (controller 'evaluate-answer (read)))
    (use-modules (guilecraft portal)
		 (modules git)
		 (profiles alex))
    ;; for now we drop into read, but we want to drop into server
    ;; listening mode.
    (read)))

;;; gdisplay is a generic name for a 
;;; procedure that will finally 
;;; produce output. For now, as CLI
;;; is the only output option, it can
;;; be defined inside this module.
;;; At a later stage it will be defined
;;; in the output module, with which
;;; Guilecraft is called.
(define gdisplay (lambda (string)
		   "Generic display procedure — could be overriden via introspection, but provides a unified standard for output in guilecraft."
		   (format #t string)
		   (newline)))

