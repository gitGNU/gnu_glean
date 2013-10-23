;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft gmodule-ops)
  #:use-module (guilecraft data-types sets)
  #:export (gmodule-full-name
	    gmodule-tags

	    car-gsets
	    cdr-gsets
	    null-gsets?
	    trad-make-gmodule))

;;; Commentary:
;;;
;;; Provide procedures related or derived from the gmodule data-type.
;;;
;;; Code:

(define (car-gsets set-contents)
  "Return the first gset in a gmodule-parts object."
  (car set-contents))
(define (cdr-gsets set-contents)
  "Return all but the first gset in a gmodule-parts-object."
  (cdr set-contents))
(define (null-gsets? set-contents)
  "Return #t if there are no gsets left in the gmodule-parts-object."
  (null? set-contents))

(define (gmodule-full-name set)
  "Return the human-friendly, full name of a SET, normally a module,
or 'unnamed' if no name exists."
  (let ((name (set-name set))
	(version (set-version set)))
    (cond ((and name version)
	   (string-append name " — version " version))
	  (name name)
	  (else "unnamed"))))

(define (gmodule-tags set)
  "Return the IDs in use in a given guilecraft set's contents, or #f
if no contents are present."
  (map set-id (set-contents set)))

(define (trad-make-gmodule i n v d l-d c p f-o-m d-s)
  (module i
    #:contents p
    #:name n
    #:version v
    #:synopsis d
    #:description l-d
    #:creator c
    #:attribution d-s
    #:resources f-o-m))
