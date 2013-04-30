;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types gmodules)
  #:use-module (guilecraft utils)
   #:export (gmod_get-id
	     gmod_make-gmodule
	    
	     gmodule-name
	     gmodule-parts
	     gmodule-version
	     gmodule-description
	     gmodule-long-description
	     gmodule-creators))

;;; Commentary:
;;;
;;; Provide a high-level mechanism to define gmodules to 
;;; be used in guilecraft.
;;;
;;; Code:

;; A gmodule.
;; Gmodules consist of meta-data and one or more sets/gmodules
(define-record-type* <gmodule>
  gmod_make-gmodule make-gmodule
  gmodule?
  (id   gmod_get-id)                   ; string
  (name gmodule-name)		  ; string
  (version gmodule-version)		       ; number
  (description gmodule-description)		; one-line descriptive string
  (long-description gmodule-long-description) ; Space for teaching material text
  (creators gmodule-creators)           ; <credit> instance
  (parts gmodule-parts)                 ; list of variables

  (find-out-more get-find-out-more) ; <materials> instance
  (derivation-source get-derivation-source) ; <materials> instance
)
