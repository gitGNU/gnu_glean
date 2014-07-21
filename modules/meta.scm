;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (store meta)
  #:use-module (guilecraft core-templates)
  #:use-module (store git)
  #:use-module (store test)
  #:export (meta-gmodule))

(define meta-gmodule
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
