;;; glean --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (glean store glean)
  #:use-module (glean library core-templates)
  #:export (glean-module))

(define tutorial
  (tutorial 'tutorial
            #:name "Glean Tutorial"
            #:version "0.1"
            #:synopsis "A basic introduction to Glean."
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

(define glean-module
  (module
   'glean
   #:name "An introduction to Glean"
   #:version "0.1"
   #:keywords '("education" "code")
   #:synopsis "Learn how Glean fits together."
   #:description "The aim of this discipline is to introduce you to the
different moving parts and concepts in Glean, to make it easier to contribute,
hack, or use it."
   #:creator "Alex Sassmannshausen"
   #:contents `(,tutorial)))
