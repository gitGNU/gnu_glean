;; boot.scm --- librarian commands launch dispatcher    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 04 November 2014
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
;;; profile including it, or activate the discipline from the store and create
;;; a new profile linking to it.
;;;;; Remove
;;; value should be a name in the store or in the current profile.  We create
;;; a new profile, with the named discipline removed from it.
;;;;; Show
;;; value should be a name in the store or in the current profile.  We display
;;; detailed information about the discipline.
;;;;; Scan
;;; show a list of all discipline paths / names in the store, with synopsis.
;;;;; Clean
;;; scan all existing profiles, then remove all disciplines from the store
;;; that are currently not linked in any profile.
;;; If value is given, it should be a profile name: delete all prior profiles
;;; and then perform the clean.
;;;;; Profile
;;; list all profiles currently in existence with a summary of the enabled
;;; disciplines.
;;; If value is given it should be a profile name: activate that profile.
(define *option-grammar*          
  '((help       (single-char #\h) (value #f))
    (usage      (single-char #\u) (value #f))
    (version    (single-char #\v) (value #f))
    (install    (single-char #\i) (value #t)) ; activate from or add to store
    ;; (remove     (single-char #\r) (value #t)) ; deactivate from store
    ;; (store-show    (single-char #\S) (value optional)) ; show discipline detail if
                                        ; value provided or
                                        ; scan store.
    ;; (clean    (single-char #\c) (value optional)) ; remove unused
                                        ; disciplines/profiles
    (catalogue (single-char #\s) (value optional)) ; list or select a
                                        ; catalogue(s)
    ;; (pick-catalogue (single-char #\p) (value optional)) ; show discipline or
                                        ; catalogue contents.
    ))

(define *messages*
  `("Show this help message and exit."
    "Show this help message and exit."
    ,(string-append "Show the version of " %glean-package-name%
                    " you are using and exit.")
    "Install the discipline located at VALUE into the store."
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
                   ((and=> (get-opt 'catalogue) boolean?)
                    (catalogue-list %catalogue-dir%))
                   ((get-opt 'catalogue)
                    (catalogue-show %catalogue-dir% (get-opt 'catalogue)))
                   (else (help))))))))

;;; boot.scm ends here
