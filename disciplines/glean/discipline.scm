;; discipline.scm --- the glean discipline -*- coding: utf-8 -*-
;;
;; This file is part of Glean.
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 20 October 2014
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
;; A discipline to introduce hackers to glean.
;;
;;; Code:

(define-module
  (glean disciplines glean discipline)
  #:use-module
  (glean disciplines glean ancestry)
  #:use-module
  (glean library core-templates)
  #:export
  (glean))


;;;; Tutorial

(define tutorial-part
  (tutorial 'tutorial
            #:name "Glean Tutorial"
            #:version "0.1"
            #:synopsis "An introduction to the conceptual landscape of Glean."
            #:chapters
            `(,(chapter "Library"
                        '("The library is the place where disciplines are
stored.")
                        "A high-level overview of the library part of Glean.")
              ,(chapter "Lounge"
                        '("The lounge is the place where user profiles and
progress is stored.")
                        "A high-level overview of the lounge part of Glean.")
              ,(chapter "Client"
                        '("The client provides one of different possible UI's
to Glean.")
                        "An overview of the client part of Glean."))))


;;;; Exercises
;;;
;;; For clarity we define the exercises at the top level.  As a result we
;;; define the discipline itself at the very bottom of the file.

(define library-part
  (set 'library
       #:contents
       (list
        (problem (q "Where are disciplines stored?")
                 (s "the library")
                 (o "the lounge")
                 (o "the client")
                 (o "the library"))
        (problem (q "‘true’ or ’false’: the library tracks the user's
progress in their chosen disciplines.")
                 (s "false")
                 (o "true")
                 (o "false"))
        (problem (q "What is the name for glean's subject areas?")
                 (s "disciplines")
                 (o "disciplines")
                 (o "subjects")
                 (o "modules"))
        (problem (q "‘true’ or ’false’: a discipline can consist of
references to further disciplines.")
                 (s "true")
                 (o "true")
                 (o "false")))))

(define lounge-part
  (set 'lounge
       #:contents
       (list
        (problem (q "User data is managed by…")
                 (s "the lounge")
                 (o "the client")
                 (o "the library")
                 (o "the lounge"))
        (problem (q "‘true‘ or ‘false‘: the lounge tracks progress of a user
across their selected disciplines.")
                 (s "true")
                 (o "false")
                 (o "true"))
        (problem (q "What part of glean is responsible for profile management?")
                 (s "the lounge")))))

(define client-part
  (set 'client
       #:contents
       (list
        (problem (q "User interfaces are provided by…")
                 (s "the client")
                 (o "the lounge")
                 (o "the client")
                 (o "the library"))
        (problem (q "The user is provided with challenges by…")
                 (s "the client"))
        (problem (q "‘true‘ or ‘false': the client communicates with both,
the library and the lounge.")
                 (s "true")
                 (o "false")
                 (o "true")))))


;;;; Discipline
;;;
;;; We have defined the exercises above, so we now we can simply define the
;;; discipline's meta data

(define glean
  (module
      'glean
      #:ancestry (ancestry-trees)
      #:name "An introduction to Glean"
      #:version "0.1"
      #:keywords '("education" "code" "glean")
      #:synopsis "Learn how Glean fits together."
      #:description "The aim of this discipline is to introduce you to the
different moving parts and concepts in Glean, to make it easier to contribute,
hack, or use it."
      #:creator "Alex Sassmannshausen"
      #:contents `(,tutorial-part ,library-part ,lounge-part ,client-part)))

;;; discipline.scm ends here
