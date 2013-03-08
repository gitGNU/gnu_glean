;;; guilecraft --- .         -*- coding: utf-8 -*-

(define-module (guilecraft gsets)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft gmodules)
  #:use-module (guilecraft open-problems)
  #:export (;; gset record functions
	    gset_gset?
	    gset_make-gset
	    gset_get-tag
	    gset_get-problems
	    gset_get-tag-problems
	    gset_get-tag-challenges 
	    gset_get-tag-solutions))

;;; 
;; Define gsets
;;;

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

;; Gset: a group of problems to be used as stream source of challenges
(define-record-type <gset>
  (gset_make-gset tag problems)
  gset_gset?
  ;; Tag: the problem subject ID against which Guilecraft
  ;; will keep 'score'
  (tag gset_get-tag)             ; symbol
  ;; Problems: this is a list of problems, generated through 
  ;; the problems interfaces.
  (problems gset_get-problems)             ; list of <problems>
  )


(define gset_get-tag-problems
  (lambda (gset-tag gmodule)
    "Return the challenge/solution pairs subsumed under TAG in a given GMODULE.

get-tag-problems searches gmodule parts and returns '() or the problems subsumed within a tag within a module."
    (define helper
      (lambda (gset-tag gmodule-tag-list)
	(cond ((null? gmodule-tag-list)
	       '())
	      ((eq? gset-tag (gset_get-tag (car gmodule-tag-list)))
	       (gset_get-problems (car gmodule-tag-list)))
	      (else (helper gset-tag (cdr gmodule-tag-list))))))
    (helper gset-tag (gmod_get-parts gmodule))))

;;;
;; Gset Helper Functions
;;;

(define get-tag-challenges
  (lambda (gset-tag gmodule)
    "Return the challenges subsumed within a tag in a given guilecraft module."
    (map op_get-challenge (gset_get-tag-problems gset-tag gmodule))))

(define get-tag-solutions
  (lambda (gset-tag gmodule)
    "Return the solutions subsumed within a tag in a given guilecraft module."
    (map op_get-solution (gset_get-tag-problems gset-tag gmodule))))

