;;; glean --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (glean store meta discipline)
  #:use-module (glean library core-templates)
  #:use-module (glean store git)
  #:use-module (glean store test)
  #:export (meta-module))

(define meta-module
  (module
    'meta
    #:name "Meta: Git and Test combined"
    #:version "0.1"
    #:keywords '("test" "recursion")
    #:synopsis "Learn to use meta"
    #:description "Long Description: background on git, introductory text"
    #:creator "Alex Sassmannshausen"
    #:attribution (list (media #:text "Git man pages & website"))
    #:contents `(,git-module ,test-module)
    #:resources (list (media #:urls '("http://www.git-scm.com")))))
