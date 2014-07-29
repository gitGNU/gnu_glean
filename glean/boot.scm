;; boot.scm --- handle launching glean    -*- coding: utf-8 -*-
;;
;; Copyright © 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 27 July 2014
;;
;; This file is part of Glean.
;;
;; Glean is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; Glean is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with glean; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; Module dealing with the boot process of glean as a whole.
;;
;; This module is part of a `family', consisting of the client, lounge,
;; library and top-level boot modules.
;;
;;; Code:

(define-module (glean boot)    
  #:use-module (glean client boot)
  #:use-module (glean lounge boot)
  #:use-module (glean library boot)
  #:use-module (glean config)
  #:use-module (glean config-utils)
  #:use-module (glean utils)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (tests test-suite)
  #:export (boot))


;;;; Variables
;;;
;;; This section defines the meta-information used by the boot logic and the
;;; standard command-line options.

(define *synopsis*
  (string-append
   "Run " %glean-package-name% ", the extensible auto-didact's swiss army \
knife.
You normally want to specifiy 'lounge', 'library' or 'client' as the
first option to launch one of its components.
"))

(define *description*
  (string-append
   "This command is a launcher for the subcommands, which should be passed to
it as the first option.
This command allows you to get some general information, or to launch one of
the subcommands.

To obtain further information about one of the subcommands (lounge, library
or client) you can run that command followed by the `--help' option."))

(define *option-grammar*
  '((help       (single-char #\h) (value #f))
    (usage      (single-char #\u) (value #f))
    (version    (single-char #\v) (value #f))
    (test-suite (single-char #\t) (value #f))))

(define *messages*
  `("Show this help message and exit."
    "Show this help message and exit."
    ,(string-append "Show the version of " %glean-package-name%
                    " you are using and exit." )
    "Run the test suite."))


;;;; Logic
;;;
;;; The following section parses the command line arguments and carries out
;;; appropriate action, such as deferring to client, lounge or library boot.

(define (boot args)
  "Set the locale, parse the options, drop into the main loop."
  (define (client? id) (or (string=? id "client") (string=? id "cln")))
  (define (lounge? id) (or (string=? id "lounge") (string=? id "lng")))
  (define (library? id) (or (string=? id "library") (string=? id "lib")))
  ;; Central initial configuration
  (setlocale LC_ALL "")                 ; sets the locale to the system locale
  (ensure-user-dirs %log-dir% %socket-dir%)

  (match args
    ((path (? client?) . rest)                   ; launch client
     (client-boot (cons path rest)))
    ((path (? lounge?) . rest)                   ; launch lounge
     (lounge-boot (cons path rest)))
    ((path (? library?) . rest)                  ; launge library
     (library-boot (cons path rest)))
    (_
     (let ((opts (getopt-long args *option-grammar*)))
       (cond ((option-ref opts 'test-suite #f)   ; --test-suite
              (run-test-suite))
             ((option-ref opts 'version #f)      ; --version
              (emit-version %glean-package-name%
                            %glean-version%))
             (else
              (emit-usage %glean-package-name%   ; --help or --usage
                          *synopsis*
                          *description*
                          *option-grammar*
                          *messages*)))))))

;;; boot.scm ends here
