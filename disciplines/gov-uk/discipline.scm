;; discipline.scm --- the gov-uk discipline -*- coding: utf-8 -*-
;;
;; This file is part of Glean.
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 06 June 2014
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
;; A discipline providing a high level overview of the http://www.gov.uk
;; website.
;;
;; Like the Koha discipline, this is primarily intended to be an illustration
;; of the current state of tutorials in Glean.
;;
;;; Code:

(define-module
  (glean disciplines gov-uk discipline)
  #:use-module
  (glean disciplines gov-uk ancestry)
  #:use-module
  (glean library core-templates)
  #:export
  (gov-uk))

(define tutorial
  (tutorial 'tutorial
            #:name "GOV.UK Tutorial"
            #:version "0.1"
            #:synopsis "Learn your way around the www.gov.uk portal: \
find out about its most important features and how to use them."
            #:chapters `(,(chapter "General Layout"
                                   '("The general layout of https://www.gov.uk/...")
                                   "We will describe the general layout of the
gov.uk website, highlighting the major regions and their uses."))
            #:completion (chapter "General Layout Tutorial Complete"
                                  '("Congratulations! You have completed the
GOV.UK General Layout tutorial!"))))

(define gov-uk
  (module
      'gov-uk
      #:ancestry (ancestry-trees)
      #:name "An Introduction To The GOV.UK Portal"
      #:keywords '("digital-literacy" "internet" "e-government")
      #:version "0.1"
      #:synopsis "Learn all you need to know to use the GOV.UK\
UK government e-services portal."
      #:description "GOV.UK is the website for the UK government. It’s\
the best place to find government services and information.  This module will\
start with a tutorial, giving you a guided tour of the salient featurs of\
GOV.UK. After this we will practice using it through targeted exercises."
      #:creator "Alex Sassmannshausen"
      #:attribution (list (media #:urls '("https://www.gov.uk/")))
      #:resources (list (media #:urls '("https://www.gov.uk/help")))
      #:logo "http://dvlaregistrations.direct.gov.uk/images/assets/gov-uk-logo-footer.jpg"
      #:contents `(,tutorial)))

;;; discipline.scm ends here
