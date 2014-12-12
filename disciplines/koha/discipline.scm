;; discipline.scm --- an introduction to Koha    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 June 2014
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
;; A discipline definition providing a high level overview of the Free library
;; management system Koha.
;;
;; This module is primarily intended as to illustrate the current state of
;; tutorials in Glean.
;;
;;; Code:

(define-module (glean disciplines koha discipline)
  #:use-module (glean library core-templates)
  #:export (koha-module))

(define basic-search
  (set 'basic-search
       #:name "Basic Searching"
       #:version "0.1"
       #:synopsis "Learn to perform quick keyword searches and
to navigate the results."
       #:contents
       (list (problem (q "After watching the video below…"
                         (media #:videos
                                '("//player.vimeo.com/video/14802047")))
                      #f)
             (problem (q "What can you do on the online catalogue?")
                      ;; (list (s "Ipso Lorum Facto <-- correct")
                      ;;       (s "Ipso Lorum Facta <-- correct"))
                      (s "Ipso Lorum Facto <-- correct")
                      (o "Ipso Lorum Facto <-- correct")
                      (o "Upso Lorum Facto")
                      (o "Ipso Lorum Facta"))
             (problem (q "Perform a quick search for 'james' at http://asdev.koha-ptfs.co.uk. Then past the resulting URL below.")
                      (s "http://asdev.koha-ptfs.co.uk/cgi-bin/koha/opac-search.pl?q=james")
                      (p string=?))
             (problem (q "To perform an 'Author Search' you should…")
                      (s "Click on the dropdown labeled 'Library Catalog' and selecting 'Author' before entering your search.")
                      (o "Click on the dropdown labeled 'Library Catalog' and selecting 'Author' before entering your search.")
                      (o "Click on Advanced search, from whence you can search by Author")
                      (o "Search first, then click on the 'Author' button.")))))

(define koha-module
  (module
    'koha
    #:name "Koha Library Catalogue"
    #:version "0.1"
    #:synopsis "Learn to use the Koha online catalogue to its full
potential."
    #:description "Koha is a fully featured, scalable library
management system. Development is sponsored by libraries of varying
types and sizes, volunteers, and support companies worldwide.  This
module will cover basic and advanced searching, placing reservations,
managing your loans and amending your details."
    #:creator "Alex Sassmannshausen"
    #:attribution
    (list
     (media #:urls '("http://koha-community.org/")))
    #:resources
    (list
     (media #:urls '("http://koha-community.org/documentation/"
                     "http://koha-community.org/documentation/other-docs/"
                     "http://koha-community.org/documentation/koha-bibliography/"
                     "http://wiki.koha-community.org/wiki/Main_Page")))
    #:logo     "http://git.koha-community.org/gitweb/Koha-logo.jpg"
    #:contents `(,basic-search)))

;;; discipline ends here
