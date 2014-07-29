;; boot.scm --- handle launching glean lounges    -*- coding: utf-8 -*-
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
;; Module dealing with the specific bootstrapping of glean lounges.
;;
;; This module is part of a `family', consisting of the client, lounge,
;; library and top-level boot modules.
;;
;;; Code:

(define-module (glean lounge boot)    
  #:use-module (ice-9 getopt-long)
  #:use-module (glean config)
  #:use-module (glean config-utils)
  #:use-module (glean profile-server)
  #:use-module (glean utils)
  #:export (lounge-boot))


;;;; Variables
;;;
;;; This section defines the meta-information used by the boot logic and the
;;; standard command-line options.

(define *synopsis*
  (string-append
   "Run a `lounge' server.  Lounge servers provide all functionality to do
with user accounts and profiles.
"))

(define *description*
  (string-append
   ""))

(define *option-grammar*          
  '((help       (single-char #\h) (value #f))
    (usage      (single-char #\u) (value #f))
    (version    (single-char #\v) (value #f))
    (listen                       (value #f))))

(define *messages*
  `("Show this help message and exit."
    "Show this help message and exit."
    ,(string-append "Show the version of " %glean-package-name%
                    " you are using and exit.")
    "Run the lounge and listen at Guile's standard port."))


;;;; Logic
;;;
;;; The following section parses the command line arguments and carries out
;;; appropriate action, such as launching the selected client.

(define (lounge-boot args)
  "Parse command line options and execute the lounge procedure."
  (let ((opts (getopt-long args *option-grammar*)))
    (cond ((option-ref opts 'version #f)      ; --version
           (emit-version %glean-package-name%
                         %glean-version%))
          ((or (option-ref opts 'usage #f)
               (option-ref opts 'help #f))
           (emit-usage %glean-package-name%   ; --help or --usage
                       *synopsis*
                       *description*
                       *option-grammar*
                       *messages*))
          (else                               ; launch Lounge
           (if (option-ref opts 'listen #f)   ; and listen?
               ((@ (system repl server) spawn-server)))
           (ensure-user-dirs %lounge-dir%)
           (ensure-config %lounge-config%)
           (load-config %lounge.conf%)
           (profile-server %lounge-port%)))))

;;; boot.scm ends here
