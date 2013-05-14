;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;; Commentary:
;;
;; Gmodule-Manager defines an interface to centrally store all known
;; gmodules. The table is indexed on gmodule-id and it returns a
;; gmodule object.
;;
;; Code:

(define-module (guilecraft gmodule-manager)
  #:use-module (guilecraft data-manager)
  #:use-module (guilecraft data-types gmodules)
  #:export  (gman_add-gmodule
	     gman_get-gmodule
	     gmodule-id->gmodule-object))

; define a data-manager instance using the result of gmod_get-id as key
(define gmodule-manager (dman_data-manager gmod_get-id))

(define gman_add-gmodule
  (lambda (gmodule-object)
    "Convenience procedure to add a given gmodule to the
gmodule-manager.

gmodule-manager is an instance of data-manager. It stores the
gmodule-object indexed by its gmodule-id, derived using gmod_get-id."
    (gmodule-manager 'put gmodule-object gmodule-object)))

(define gman_get-gmodule
  (lambda (gmodule-id)
    "Convenience procedure to retrieve a given gmodule from the active
gmodule-table stored in gmodule-manager."
    (gmodule-manager 'get gmodule-id)))

; gmodule-id->gmodule-object is in this module, not gmodule-ops,
; because it operates at a higher level: it applies through
; gmodule-manager, not directly through gmodules.
(define gmodule-id->gmodule-object
  (lambda (gmodule-id)
    "Return the gmodule that is named by gmodule-id."
    (gman_get-gmodule gmodule-id)))
