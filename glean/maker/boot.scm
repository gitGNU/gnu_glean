;; boot.scm --- handle launching glean maker -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 26 October 2014
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
;; Module dealing with the specific bootstrapping of glean maker commands.
;;
;; This module is part of a `family', consisting of the client, lounge,
;; library, maker and top-level boot modules.
;;
;;; Code:

(define-module (glean maker boot)
  #:use-module (glean config)
  #:use-module (glean common config-utils)
  #:use-module (glean common utils)
  #:use-module (glean maker source)
  #:use-module (ice-9 getopt-long)
  #:export (maker-boot))


;;;; Variables
;;;
;;; This section defines the meta-information used by the boot logic and the
;;; standard command-line options.

(define *synopsis*
  (string-append
   "Run a command that is part of the `maker' command-line authoring toolset.
Maker is used primarily by content authors and maintainers.
"))

(define *description*
  (string-append
   ""))

(define *option-grammar*
  '((help       (single-char #\h) (value #f))
    (force      (single-char #\f) (value #f))
    (usage      (single-char #\u) (value #f))
    (version    (single-char #\v) (value #f))
    (cast       (single-char #\c) (value #f))
    (engrave    (single-char #\e) (value #f))
    (source     (single-char #\s) (value #t))))

(define *messages*
  `("Show this help message and exit."
    "Perform operations non-cautiously."
    "Show this help message and exit."
    ,(string-append "Show the version of " %glean-package-name%
                    " you are using and exit.")
    "Prepare a package of the discipline defined in the current directory."
    "Compose the ancestry file the current directory's discipline."
    "Prepare a skeleton discipline in the current directory, named VALUE."))

(define (help)
  (emit-usage (string-downcase %glean-package-name%)
              *synopsis*
              *description*
              *option-grammar*
              *messages*
              #:subcommand "maker | mkr"))


;;;; Logic
;;;
;;; The following section parses the command line arguments and carries out
;;; appropriate action, such as packaging the current directory's discipline.

(define (maker-boot args)
  "Parse command line options and execute the maker procedure."
  (let ((opts (getopt-long args *option-grammar*)))
    (cond ((option-ref opts 'version #f)      ; --version
           (emit-version %glean-package-name%
                         %glean-version%))
          ((or (option-ref opts 'usage #f) ; --help or --usage
               (option-ref opts 'help #f))
           (help))
          (else                               ; launch maker
           (if (option-ref opts 'source #f)
               (source (option-ref opts 'source #f)
                       (option-ref opts 'force #f))
               (help))))))

;;; boot.scm ends here
