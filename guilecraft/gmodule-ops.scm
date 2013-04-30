;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft gmodule-ops)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft data-types gsets)
  #:export (gmodule-full-name
	    gmod_get-gmodule-tags))

;;; Commentary:
;;;
;;; Provide procedures related or derived from the gmodule data-type.
;;;
;;; Code:

(define gmodule-full-name
  (lambda (gmodule)
  "Return the full name of GMODULE--i.e., `NAME — version VERSION'."
    (string-append (gmodule-name gmodule) " — version " (gmodule-version gmodule))))

(define gmod_get-gmodule-tags
  (lambda (gmodule)
    "Return the tags in use in a given guilecraft module."
    (map gset_get-tag (gmodule-parts gmodule))))
