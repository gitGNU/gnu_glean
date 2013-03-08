;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft gmodules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (;; gmodule record functions
	    gmod_make-gmodule
	    gmod_get-parts
            ))

;;; Commentary:
;;;
;;; This module provides a high-level mechanism to define gmodules to 
;;; be used in guilecraft.
;;;
;;; Code:

;; A gmodule.
;; Gmodules consist of meta-data and one or more sets/gmodules
(define-record-type <gmodule>
  (gmod_make-gmodule id name version description long-description creators find-out-more derivation-source parts)
  gmodule?
  (id get-id)		     		; short symbol id
  ;; What's the gmodule called?
  (name get-name)		  ; string
  ;; Version should really be a number
  (version get-version)		       ; number
  ;; First introduction for users
  (description get-description)		; one-line descriptive string
  ;; Tutorial & background
  (long-description get-long-description) ; Space for teaching material text
  ;; Credit for work done
  (creators get-creators)           ; <credit> instance
  ;; Further Reading
  (find-out-more get-find-out-more) ; <materials> instance
  ;; Give a shout out to your inspiration
  (derivation-source get-derivation-source) ; <materials> instance
  ;; 'Contents' of your gmodule: 'external' gmodules and 'internal' sets
  (parts gmod_get-parts)                 ; list of variables
)

;;;
;; Gmodule Helper Functions
;;;

(define get-gmodule-full-name 
  (lambda (gmodule)
  "Return the full name of GMODULE--i.e., `NAME — version VERSION'."
    (string-append (get-name gmodule) " — version " (get-version gmodule))))

(define get-gmodule-tags 
  (lambda (gmodule)
    "Return the tags in use in a given guilecraft module."
    (map gset_get-tag (gmod_get-parts gmodule))))
