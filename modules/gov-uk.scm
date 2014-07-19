;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (store gov-uk)
  #:use-module (guilecraft core-templates)
  #:export (gov-uk-module))

(define layout
  (tutorial 'layout
            #:name "GOV.UK General Layout"
            #:contents
            (list (problem (q "The general layout of..."
                              (media #:urls
                                     '("https://www.gov.uk/")))
                           #f))))
(define complete
  (tutorial 'complete
            #:name "GOV.UK Completed Tutorial"
            #:contents
            (list (problem (q "Congratulations! You have completed the
GOV.UK tutorial!"
                              (media #:images
                                     '("http://localhost/images/ptfs/tick.png")))
                           #f))))

(define tutorial
  (tutorial 'tutorial
            #:name "GOV.UK Tutorial"
            #:version "0.1"
            #:synopsis "Learn your way around the www.gov.uk portal: \
find out about its most important features and how to use them."
            #:contents
            (list layout complete)))

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
    #:attribution
    (list
     (media #:urls '("https://www.gov.uk/")))
    #:resources
    (list
     (media #:urls '("https://www.gov.uk/help")))
    #:logo "http://dvlaregistrations.direct.gov.uk/images/assets/gov-uk-logo-footer.jpg"
    #:contents `(,tutorial)))
