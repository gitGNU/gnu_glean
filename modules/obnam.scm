;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (modules obnam)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft problem-types open-problems)
  #:use-module (guilecraft problem-types multi-choice-problems)
  #:use-module (guilecraft gmodule-manager)
  #:export (obnam-gmodule))


(define obnam-backup
  (gset_make-gset 'obnam-backup
		  `(,(op_make-open-problem "How would you backup ~/Documents in /media/backups?"
					   "obnam backup ~/Documents  /media/backups"))))

(define obnam-gmodule
  (gmodule
   (id 'obnam)
   (name "Obnam: easy backup")
   (version "0.1")
   (description "Learn to backup your files with Obnam.")
   (long-description "Long Description: background on obnam, introductory text")
   (creators "Alex Sassmannshausen")
   (find-out-more "http://www.liw-fi.com/obnam")
   (derivation-source "Obnam man pages & website")
   (parts `(,obnam-backup))))

(gman_add-gmodule obnam-gmodule)
