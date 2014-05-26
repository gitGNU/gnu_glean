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
  #:use-module (guilecraft config)
  #:use-module (guilecraft module-server)
  #:use-module (guilecraft profile-server)
  #:use-module (guilecraft library-store)
  #:use-module (guilecraft clients repl)
  #:use-module (guilecraft clients cli)
  #:use-module (guilecraft clients web)
  #:use-module (guilecraft utils)
  #:export (boot))

(define clients `((repl . ,repl-client)
                  (web  . ,web-client)))
(define (client? value)
  (if (boolean? value)
      value
      (assq-ref clients (string->symbol value))))

;; Define the list of accepted options and their special properties
(define *option-grammar* `((client (single-char #\c) (value optional)
                                   (predicate ,client?))
                           (config (value #t))
                           (export (single-char #\e) (value #t))
                           (help (single-char #\h) (value #f))
                           (import (single-char #\i) (value #t))
                           (listen (value #f))
                           (module-server (single-char #\m) (value #f))
                           (profile-server (single-char #\p) (value #f))
                           (remove (single-char #\r) (value #t))
                           (test-suite (single-char #\t) (value #f))
                           (usage (single-char #\u) (value #f))
                           (version (single-char #\v) (value #f))))

(define usage
  (lambda ()
    "Dispatch a usage message, with permitted command-line options, to gdisplay for output."
    (define repr-option 
      (lambda (opt)
        "Return, as string the car of OPT.
Options will be surrounded by square brackets if optional."
        (string-append "[--" (object->string (car opt)) "]")))
    (format #t "usage: guilecraft ~a \n"
            (string-join (map repr-option *option-grammar*)))
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
        (begin (run-test-suite)
               (exit 0)))
    (if (option-ref opts 'test-server #f)
        (begin
          (run-server-tests)
          (exit 0)))
    (if (option-ref opts 'version #f)
        (begin
          (version)
          (exit 0)))
    (if (option-ref opts 'listen #f)
        ((@ (system repl server) spawn-server)))
    opts)))

(define (boot args)
  "Set the locale, parse the options, drop into the main loop."
  (setlocale LC_ALL "")                ; sets the locale to the system locale
  (let ((options (parse-options args))
        (start-clock (current-time)))
    (let ((config (option-ref options 'config #f)))
      (if config (load-config config))
      (ensure-user-dirs %log-dir% %socket-dir% %library-dir%
                        %lounge-dir% %wip-library-dir%
                        %bak-library-dir%)

      (cond ((option-ref options 'module-server #f)
             (ensure-config %library-config%)
             (load-config %library.conf%)
             (module-server %library-port%))
            ((option-ref options 'profile-server #f)
             (ensure-config %lounge-config%)
             (load-config %lounge.conf%)
             (profile-server %lounge-port%))
            ((option-ref options 'import #f)
             (import-module (option-ref options 'import #f)))
            ((option-ref options 'remove #f)
             (remove-module (option-ref options 'remove #f)))
            ((option-ref options 'export #f)
             (export-module (option-ref options 'export #f)))
            (else (ensure-config %client-config%)
                  (load-config %client.conf%)
                  (client (option-ref options 'client #f)
                          %default-client%))))))

(define (client client default)
  (let ((selected (if (boolean? client)
                      default
                      (string->symbol client))))
    (format #t "Client: ~a\nDefault: ~a\n" selected default)
    ((assq-ref clients selected))))

;;; Just a place-holder
(define (main-loop)
  "The main loop which calls challenges and expects answers, until the kill signal."
  (begin
    ;; for now we drop into read, but we want to drop into server
    ;; listening mode.
    (read)))

(define (ensure-user-dirs . dirs)
  (for-each mkdir-p dirs))

(define (ensure-config config)
  (if (access? (config-target config) R_OK)
      (format #t "~a configuration exists.\n" (config-name config))
      (begin
        (format #t "~a configuration is being created… "
                (config-name config))
        (config-write config)
        (format #t "[Done]\n"))))

(define (load-config config)
  ;; Parse optional root config-file
  (let ((config-module (resolve-module '(guilecraft config))))
    (save-module-excursion
     (lambda ()
       (set-current-module config-module)
       (primitive-load config)))))
