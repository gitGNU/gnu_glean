;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (modules obnam)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft problem-types open-problems)
  #:use-module (guilecraft problem-types multi-choice-problems)
  #:export (obnam-gmodule))


(define obnam-backup
  (make-gset 'obnam-backup
		  `(,(make-open-problem "How would you backup ~/Documents in /media/backups?"
					   "obnam backup ~/Documents  /media/backups"))))

(define obnam-gmodule
  (gmodule
   (id 'obnam)
   (name "Obnam: easy backup")
   (version "0.1")
   (synopsis "Learn to backup your files with Obnam.")
   (description "Long Description: background on obnam, introductory text")
   (creators "Alex Sassmannshausen")
   (find-out-more "http://www.liw-fi.com/obnam")
   (derivation-source "Obnam man pages & website")
   (parts `(,obnam-backup))))
