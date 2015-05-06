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
  #:use-module (glean maker engrave)
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
    (usage      (single-char #\u) (value #f))
    (version    (single-char #\v) (value #f))
    (log        (single-char #\l) (value optional))
    (log-level  (single-char #\L) (value #t)
                (predicate ,(lambda (value)
                              (or (boolean? value)
                                  (memv (string->symbol value)
                                        (log-levels))))))
    (verbose    (single-char #\V) (value #f))
    (cast       (single-char #\c) (value #f))
    (force      (single-char #\f) (value #f))
    (engrave    (single-char #\e) (value optional))
    (print      (single-char #\p) (value #f))
    (source     (single-char #\s) (value #t))))

(define *messages*
  `("Show this help message and exit."
    "Show this help message and exit."
    ,(string-append "Show the version of " %glean-package-name%
                    " you are using and exit.")
    "Set log file to VALUE or default and enable logging."
    ,(string-append "Set log-level to VALUE (choose from:"
                    (string-join (map symbol->string (log-levels)) ", ") ").")
    "Enable logging to stdout."
    "Prepare a package of the discipline defined in the current directory."
    "Perform operations non-cautiously."
    "Compose the ancestry file the current directory's discipline."
    "Instead of writing output to files, print output to stdout."
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
    (define (get-opt what) (option-ref opts what #f))
    (cond ((get-opt 'version) (emit-version %glean-package-name%
                                            %glean-version%))
          ((or (get-opt 'usage) (get-opt 'help)) (help))
          (else                               ; launch maker
           (parameterize ((log-level (if (string? (get-opt 'log-level))
                                         (string->symbol (get-opt 'log-level))
                                         %log-level%))
                          (logger    (make-logger (get-opt 'verbose)
                                                  (get-opt 'log)
                                                  %log-file%)))
             (cond ((get-opt 'source)
                    (source (get-opt 'source) (get-opt 'force)
                            #:print (get-opt 'print)))
                   ((get-opt 'engrave)
                    (engrave (get-opt 'engrave) %current-catalogue%
                             %catalogue-dir% #:print (get-opt 'print)))
                   (else (help))))))))

;;; boot.scm ends here
