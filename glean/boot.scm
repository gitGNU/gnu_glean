;; boot.scm --- handle launching glean    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 27 July 2014
;;
;; This file is part of Glean.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Module dealing with the boot process of glean as a whole.
;;
;; This module is part of a `family', consisting of the client, lounge,
;; library and top-level boot modules.
;;
;;; Code:

(define-module (glean boot)
  #:autoload (glean client boot) (client-boot)
  #:autoload (glean library boot) (library-boot)
  #:autoload (glean lounge boot) (lounge-boot)
  #:autoload (glean librarian boot) (librarian-boot)
  #:autoload (glean maker boot) (maker-boot)
  #:use-module (glean config)
  #:use-module (glean common config-utils)
  #:use-module (glean common utils)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:export (boot))


;;;; Variables
;;;
;;; This section defines the meta-information used by the boot logic and the
;;; standard command-line options.

(define *synopsis*
  (string-append
   "Run " %glean-package-name% ", the extensible auto-didact's swiss army \
knife.
You normally want to specifiy 'lounge', 'library', 'client' or 'maker' as the
first option to launch one of its components.
"))

(define *description*
  (string-append
   "This command is a launcher for the subcommands, which should be passed to
it as the first option.
This command allows you to get some general information, or to launch one of
the subcommands.

To obtain further information about one of the subcommands (lounge, library,
client or maker) you can run that command followed by the `--help' option."))

(define *option-grammar*
  '((help       (single-char #\h) (value #f))
    (usage      (single-char #\u) (value #f))
    (version    (single-char #\v) (value #f))))

(define *messages*
  `("Show this help message and exit."
    "Show this help message and exit."
    ,(string-append "Show the version of " %glean-package-name%
                    " you are using and exit." )))


;;;; Logic
;;;
;;; The following section parses the command line arguments and carries out
;;; appropriate action, such as deferring to client, lounge or library boot.

(define (boot args)
  "Set the locale, parse the options, drop into the main loop."
  (define (client? id)  (or (string=? id "client") (string=? id "cln")))
  (define (lounge? id)  (or (string=? id "lounge") (string=? id "lng")))
  (define (library? id) (or (string=? id "library") (string=? id "lib")))
  (define (librarian? id) (or (string=? id "librarian") (string=? id "lbr")))
  (define (maker? id)   (or (string=? id "maker") (string=? id "mkr")))
  (catch 'system-error                  ; Install the locale
    (lambda _
      (setlocale LC_ALL ""))
    (lambda args
      (warning (_ "failed to install locale: ~a~%")
               (strerror (system-error-errno args)))))
  (textdomain %gettext-domain%)         ; Setup gettext
  (ensure-user-dirs %log-dir% %socket-dir%) ; Create user dirs.

  ;; FIXME: despite the use of #:autoload in the module definition, the use of
  ;; client-, lounge-, and library-boot cause the entire module dependency of
  ;; Glean to be loaded when this boot is first executed.  It seem to me, at
  ;; present, that this is an implementation error in Guile?
  ;;
  ;; Perhaps this is due to the use of match instead of if statements?
  (match args
    ((path (? client?) . rest)          ; launch client
     (parameterize ((program-name "client"))
       (client-boot (cons path rest))))
    ((path (? lounge?) . rest)          ; launch lounge
     (parameterize ((program-name "lounge"))
       (lounge-boot (cons path rest))))
    ((path (? library?) . rest)         ; launch library
     (parameterize ((program-name "library"))
       (library-boot (cons path rest))))
    ((path (? librarian?) . rest)
     (parameterize ((program-name "librarian"))
       (librarian-boot (cons path rest))))
    ((path (? maker?) . rest)           ; launch maker
     (parameterize ((program-name "maker"))
       (maker-boot (cons path rest))))
    (_
     (let ((opts (getopt-long args *option-grammar*)))
       (cond ((option-ref opts 'version #f)      ; --version
              (emit-version %glean-package-name%
                            %glean-version%))
             (else
              (emit-usage %glean-package-name%   ; --help or --usage
                          *synopsis*
                          *description*
                          *option-grammar*
                          *messages*)))))))

;;; boot.scm ends here
