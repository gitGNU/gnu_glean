;; discipline.scm --- introduction to gov.uk    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 06 June 2014
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
;; A discipline providing a high level overview of the http://www.gov.uk
;; website.
;;
;; Like the Koha discipline, this is primarily intended to be an illustration
;; of the current state of tutorials in Glean.
;;
;;; Code:

(define-module (glean disciplines gov-uk discipline)
  #:use-module (glean library core-templates)
  #:export (gov-uk-module))

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

(define gov-uk-module
  (module
    'gov-uk
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

;;; discipline ends here
