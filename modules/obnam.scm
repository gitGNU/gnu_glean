;;; -*- coding:utf-8 -*-

(define-module (modules obnam)
  #:use-module (guilecraft gmodules)
  #:export (obnam-gmodule))


(define obnam-backup
  (make-gset 'obnam-backup
	     `(,(make-open-problem "How would you backup ~/Documents in /media/backups?"
				  "obnam backup ~/Documents  /media/backups"))))

(define obnam-gmodule
  (make-gmodule "Obnam: easy backup"
		"0.1"
		"Learn to backup your files with Obnam."
		"Long Description: background on obnam, introductory text"
		"Alex Sassmannshausen"
		"http://www.liw-fi.com/obnam"
		"Obnam man pages & website"
		`(,obnam-backup)))
