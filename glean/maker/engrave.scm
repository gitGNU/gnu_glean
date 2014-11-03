;; engrave.scm --- implement ancestry auto-generation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 02 November 2014
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
;; Engrave provides functionality whereby a discipline's ancestry file is
;; auto-generated.  By default it should scan the current directory for the
;; top-level discipline definition and the existing ancestry file.  It should
;; then traverse the discipline, taking into account the existing ancestry
;; file, the previous version of the discipline currently in the store and
;; this discipline's definition (in particular the lineage fields), to
;; generate a new ancestry file.
;;
;; The first version will not take existing modules in the store into account.
;; A later version will accept a command-line argument pointing to a
;; discipline folder, which is the one to be processed.
;;
;;; Code:

(define-module (glean maker engrave)
  #:use-module (glean common utils)
  #:use-module (glean library core-templates)
  #:use-module (glean library sets)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-2)
  #:export     (engrave))


;;;; Porcelain

(define %mkr-discipline% "discipline.scm")

(define (engrave target)
  (and-let* ((discipline-filename (parse-input target))
             (discipline-dirname (dirname discipline-filename))
             ((add-to-load-path discipline-dirname)))
            (parse-discipline
             (module-public-interface (begin (primitive-load discipline-filename)
                                             (current-module)))))
  (exit 0))

(define (parse-discipline discipline-interface)
  (module-map (lambda (name value)
                (advice (_ "Analysing ~a.~%") name)
                (match (resolve-set (variable-ref value))
                  ((? set? set) (format #t "Yay: ~a.~%" (set-id set)))
                  (otherwise (format #t "Nay: ~a.~%" otherwise))))
              discipline-interface))

(define (parse-input input)
  (match input
    ((? string? input)
     (or (and-let* (((and (file-exists? input) (file-is-directory? input)))
                    (total (string-append
                            (if (absolute-file-name? input)
                                input
                                (string-append (getcwd)
                                               file-name-separator-string
                                               input))
                            file-name-separator-string
                            %mkr-discipline%))
                    ((file-exists? total)))
                   (advice (_ "Engraving '~a' (~a).~%") input total)
                   total)
         (report-error (_ "~a is not a directory, does not exist or does not
contain a file named '~a'.~%") input %mkr-discipline%)))
    (#t
      (or (and-let* ((total (string-append (getcwd)
                                           file-name-separator-string
                                           %mkr-discipline%))
                     ((file-exists? total)))
                    (advice (_ "Engraving '~a' in the current directory (~a).~%")
                            %mkr-discipline% total)
                    total)
          (report-error (_ "The current directory does not contain a file
named '~a'.~%") %mkr-discipline%)))
    (_ (error "ENGRAVE -- Unexpected INPUT" input))))

;;; engrave.scm ends here
