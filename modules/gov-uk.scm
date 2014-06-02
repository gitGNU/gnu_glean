;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (store gov-uk)
  #:use-module (guilecraft data-types sets)
  #:export (gov-uk-module))

(define basic-search
  (set 'tutorial
       #:name "www.gov.uk Tutorial"
       #:version "0.1"
       #:synopsis "Learn your way around the www.gov.uk portal: \
find out about its most important features and how to use them."
       #:contents
       (list (problem (q "The general layout of..."
                         (media #:urls
                                '("https://www.gov.uk/")))
                      #f))))

(define gov-uk-module
  (module
    'gov-uk
    #:name "An Introduction To The GOV.UK Portal"
    #:version "0.1"
    #:synopsis "Learn all you need to know to use the GOV.UK\
UK government e-services portal."
    #:description "GOV.UK is the website for the UK government. It’s\
the best place to find government services and information.  This module will\
start with a tutorial, giving you a guided tour of the salient featurs of\
GOV.UK. After this we will practice using it through targeted exercises."
    #:creator "Alex Sassmannshausen"
    #:attribution
    (list
     (media #:urls '("https://www.gov.uk/")))
    #:resources
    (list
     (media #:urls '("https://www.gov.uk/help")))
    #:logo
    "https://assets.digital.cabinet-office.gov.uk/static/gov.uk_logotype_crown-c09acb07e4d1d5d558f5a0bc53e9e36d.png"
    #:contents `(,basic-search)))
