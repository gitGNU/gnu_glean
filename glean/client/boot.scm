;; boot.scm --- handle launching glean clients    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
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
;; Module dealing with the specific bootstrapping of glean clients.
;;
;; This module is part of a `family', consisting of the client, lounge,
;; library and top-level boot modules.
;;
;;; Code:

(define-module (glean client boot)    
  #:use-module (ice-9 getopt-long)
  #:use-module (glean config)
  #:use-module (glean common components)
  #:use-module (glean common config-utils)
  #:use-module (glean common utils)
  #:export (client-boot))


;;;; Variables
;;;
;;; This section defines the meta-information used by the boot logic and the
;;; standard command-line options.

(define *synopsis*
  (string-append
   "Run a `client'.  Clients provide the interface through which users can
access disciplines, study and manage their profiles.
If no further options are specified, we will simply start the default client
specified in the main configuration file.
")) 

(define *description*
  (string-append
   ""))

(define *option-grammar*
  '((help       (single-char #\h) (value #f))
    (usage      (single-char #\u) (value #f))
    (version    (single-char #\v) (value #f))
    (client     (single-char #\c) (value optional))
    (listen                       (value #f))))

(define *messages*
  `("Show this help message and exit."
    "Show this help message and exit."
    ,(string-append "Show the version of " %glean-package-name%
                    " you are using and exit.")
    "Run the client specified as this option."
    "Run the default client and listen at Guile's standard port."))


;;;; Logic
;;;
;;; The following section parses the command line arguments and carries out
;;; appropriate action, such as launching the selected client.

(define (client-boot args)
  "Parse command line options and execute the client procedure or other
actions requested."
  (let ((opts (getopt-long args *option-grammar*)))
    (cond ((option-ref opts 'version #f)      ; --version
           (emit-version %glean-package-name%
                         %glean-version%))
          ((or (option-ref opts 'usage #f) ; --help or --usage
               (option-ref opts 'help #f))
           (emit-usage (string-downcase %glean-package-name%)
                       *synopsis*
                       *description*
                       *option-grammar*
                       *messages*
                       #:subcommand "client | cln"))
          (else                               ; launch client
           (if (option-ref opts 'listen #f)   ; and listen?
               ((@ (system repl server) spawn-server)))
           ;; Global client features
           (ensure-user-dirs %client-dir%)
           (ensure-config    %client-config%)
           (load-config      %client.conf%)
           ;; Simply apply the thunk returned by client.
           ((get-client %glean-dir% %user-dir%
                        (if (option-ref opts 'client #f)
                            (option-ref opts 'client #f)
                            %default-client%)))))))

(define (get-client core-root extensions-root default)
  "Return the procedure defined by client that specified by DEFAULT after
loading pre-loading all clients at CORE-ROOT and EXTENSIONS-ROOT."
  ((component-node core-root
                   (string-append core-root "/glean/client/components")
                   extensions-root
                   (string-append extensions-root "/clients")
                   default)
   'get))

;;; boot.scm ends here
