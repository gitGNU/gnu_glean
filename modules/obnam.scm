;;; -*- coding:utf-8 -*-

(define-module (modules obnam)
  #:use-module (guilecraft gmodules)
  #:use-module (guilecraft gsets)
  #:use-module (guilecraft types open-problems)

  #:export (obnam-gmodule))


(define obnam-backup
  (gset_make-gset 'obnam-backup
		  `(,(op_make-open-problem "How would you backup ~/Documents in /media/backups?"
					   "obnam backup ~/Documents  /media/backups"))))

(define obnam-gmodule
  (gmod_make-gmodule 'obnam
		     "Obnam: easy backup"
		     "0.1"
		     "Learn to backup your files with Obnam."
		     "Long Description: background on obnam, introductory text"
		     "Alex Sassmannshausen"
		     "http://www.liw-fi.com/obnam"
		     "Obnam man pages & website"
		     `(,obnam-backup)))
