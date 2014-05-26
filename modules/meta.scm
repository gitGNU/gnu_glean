;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (library meta)
  #:use-module (guilecraft data-types sets)
  #:use-module (library git)
  #:use-module (library test)
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
    #:contents `(,git-gmodule ,test-module)
    #:resources (list (media #:urls '("http://www.git-scm.com")))))
