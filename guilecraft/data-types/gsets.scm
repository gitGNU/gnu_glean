;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types gsets)
  #:use-module (srfi srfi-9)
  #:export (gset_gset?
	    gset_make-gset
	    gset_get-tag
	    gset_get-problems))

;;; 
;; Define gsets
;;;
;; Gsets are a means of grouping problems together under a
;; tag. Guilecraft uses the tag to measure progress against a
;; discipline, and will select a "random" question from the
;; algorithmically determined highest priority gset. The overall
;; topography that should be starting to manifest is one where
;; disciplines are provided by gmodules, which either point to other
;; gmodules (to make use of content that has already been created), or
;; which contain a number of gsets that then allow guilecraft to
;; select problems.

(define-record-type <gset>
  (gset_make-gset tag problems)
  gset_gset?
  (tag gset_get-tag)
  (problems gset_get-problems))