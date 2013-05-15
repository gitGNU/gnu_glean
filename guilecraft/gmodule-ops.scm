;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft gmodule-ops)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft data-types gsets)
  #:export (gmodule-full-name
	    gmodule-tags

	    car-gsets
	    cdr-gsets
	    null-gsets?))

;;; Commentary:
;;;
;;; Provide procedures related or derived from the gmodule data-type.
;;;
;;; Code:

(define (car-gsets gmodule-parts-object)
  "Return the first gset in a gmodule-parts object."
  (car gmodule-parts-object))
(define (cdr-gsets gmodule-parts-object)
  "Return all but the first gset in a gmodule-parts-object."
  (cdr gmodule-parts-object))
(define (null-gsets? gmodule-parts-object)
  "Return #t if there are no gsets left in the gmodule-parts-object."
  (if (eq? '() gmodule-parts-object)
      #t
      #f))

(define gmodule-full-name
  (lambda (gmodule)
  "Return the full name of GMODULE--i.e., `NAME — version VERSION'."
    (string-append (gmodule-name gmodule) " — version " (gmodule-version gmodule))))

(define gmodule-tags
  (lambda (gmodule)
    "Return the tags in use in a given guilecraft module."
    (map gset_get-tag (gmodule-parts gmodule))))
