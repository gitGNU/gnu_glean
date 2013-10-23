;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;; Commentary:
;;
;; Gmodule-Manager defines an interface to centrally store all known
;; gmodules. The table is indexed on set-id and it returns a
;; gmodule object.
;;
;; Code:

(define-module (guilecraft gmodule-manager)
  #:use-module (guilecraft data-manager)
  #:use-module (guilecraft utils)
  #:use-module (rnrs)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft gmodule-ops)
  #:export  (gman_add-gmodule
	     gman_get-gmodule
	     gman_list-gmodules
	     set-id->gmodule-object
	     set-id->set-object))

; define a data-manager instance using the result of set-id as key
(define gmodule-manager (dman_data-manager set-id))

(define gman_add-gmodule
  (lambda (gmodule-object)
    "Convenience procedure to add a given gmodule to the
gmodule-manager.

gmodule-manager is an instance of data-manager. It stores the
gmodule-object indexed by its set-id, derived using set-id."
    (gmodule-manager 'put gmodule-object gmodule-object)))

(define gman_get-gmodule
  (lambda (set-id)
    "Convenience procedure to retrieve a given gmodule from the active
gmodule-table stored in gmodule-manager."
    (gmodule-manager 'get set-id)))

(define (gman_list-gmodules)
  (gmodule-manager 'list))

; set-id->gmodule-object is in this module, not gmodule-ops,
; because it operates at a higher level: it applies through
; gmodule-manager, not directly through gmodules.
(define set-id->gmodule-object
  (lambda (set-id)
    "Return the gmodule that is named by set-id."
    (gman_get-gmodule set-id)))

(define (set-id->set-object module-id setid)
  "Return the set in MODULE-ID identified by SET-ID. Return #f if the
set cannot be found."
  (define (find-set contents)
    "Return the set in CONTENTS identified by SET-ID or #f."
    (cond ((null? contents)
	   #f)
	  ((eqv? (set-id (car-gsets contents)) setid)
	   (car-gsets contents))
	  (else (find-set (cdr-gsets contents)))))

  (find-set (set-contents (set-id->gmodule-object module-id))))
