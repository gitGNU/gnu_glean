;; boot.scm --- librarian commands launch dispatcher    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 04 November 2014
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
;; Module dealing with the specific bootstrapping of the glean librarian
;; subcommands.
;;
;; This module is part of a `family' of boot scripts for the various
;; subcommands of glean.
;;
;;; Code:

(define-module (glean librarian boot)    
  #:use-module (glean config)
  #:use-module (glean common config-utils)
  #:use-module (glean common utils)
  #:use-module (glean librarian catalogues)
  ;;  #:use-module (glean lounge librarian-ui)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (librarian-boot))


;;;; Variables
;;;
;;; This section defines the meta-information used by the boot logic and the
;;; standard command-line options.

(define *synopsis*
  (string-append
   "Run a `librarian' subcommand. Librarian subcommands are used to administer
the library.
"))

(define *description*
  (string-append
   "`librarian' subcommands are further subdivided into catalogue subcommands
and store subcommands.  The former affect only the currently active
disciplines.  The latter affect all disciplines known to Glean (e.g. different
versions of the same discipline)."))

;;;;; Install
;;; value should be either a path to a 'discipline file' or a name in the
;;; store.  We either add the discipline file to the store, then create a new
;;; catalogue including it, or activate the discipline from the store and
;;; create a new catalogue linking to it.
;;;;; Remove
;;; value should be a name in the store or in the current catalogue.  We
;;; create a new catalogue, with the named discipline removed from it.
;;;;; Store-Show
;;; value should be a name in the store or in the current catalogue.  We
;;; display detailed information about the discipline.  If no value is
;;; provided, list summaries of all installed disciplines.
;;;;; Clean
;;; scan all existing catalogues, then remove all disciplines from the store
;;; that are currently not linked in any catalogue.  If value is given, it
;;; should be a catalogue name: delete all prior catalogues and then perform
;;; the clean.
;;;;; Catalogue-Show
;;; value should be a catalogue name in the catalogues directory.  We display
;;; detailed information about that catalogue.  If no value is provided, show
;;; a summary of all existing catalogues.
(define *option-grammar*          
  '((help       (single-char #\h) (value #f))
    (usage      (single-char #\u) (value #f))
    (version    (single-char #\v) (value #f))
    (install    (single-char #\i) (value #t))
    (remove     (single-char #\r) (value #t))
    ;; (store-show (single-char #\S) (value optional))
    ;; (clean    (single-char #\c) (value optional))
    (catalogue-show (single-char #\s) (value optional))
    ;; (pick-catalogue (single-char #\p) (value optional))
    ))

(define *messages*
  `("Show this help message and exit."
    "Show this help message and exit."
    ,(string-append "Show the version of " %glean-package-name%
                    " you are using and exit.")
    "Install the discipline located at VALUE into the store."
    "Remove the discipline identified by VALUE from your active catalogue."
    "Show information about the catalogue VALUE, or list all catalogues."))



;;;; Logic
;;;
;;; The following section parses the command line arguments and carries out
;;; appropriate action, such as installing a new discipline in the store.

(define (librarian-boot args)
  "Parse command line options and execute the appropriate store subcommand."
  (define (help)
    (emit-usage (string-downcase %glean-package-name%)
                *synopsis*
                *description*
                *option-grammar*
                *messages*
                #:subcommand "librarian | lbr"))
  
  (let ((opts (getopt-long args *option-grammar*)))
    (define (get-opt what) (option-ref opts what #f))
    (cond ((get-opt 'version)
           (emit-version %glean-package-name %glean-version%))
          ((or (get-opt 'usage) (get-opt 'help))
           (help))
          (else
           (ensure-user-dirs %library-dir% %catalogue-dir%)
           (ensure-config %library-config%)
           (load-config %library.conf%)
           (parameterize ((log-level (if (string? (get-opt 'log-level))
                                         (string->symbol (get-opt 'log-level))
                                         %log-level%))
                          (logger    (make-logger (get-opt 'verbose)
                                                  (get-opt 'log)
                                                  %log-file%)))

             (cond ((get-opt 'install)
                    (catalogue-install %catalogue-dir% %current-catalogue%
                                       %library-dir% (get-opt 'install)))
                   ((get-opt 'remove)
                    (catalogue-remove %catalogue-dir% %current-catalogue%
                                      (get-opt 'remove)))
                   ((and=> (get-opt 'catalogue-show) boolean?)
                    (catalogue-list %catalogue-dir%))
                   ((get-opt 'catalogue-show)
                    (catalogue-show %catalogue-dir%
                                    (get-opt 'catalogue-show)))
                   (else (help))))))))

;;; boot.scm ends here
