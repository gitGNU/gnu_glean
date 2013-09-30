;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft whirligigs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft gset-ops)
  #:use-module (guilecraft gmodule-manager)
  #:export (make-whirligig
	    hangar))

;;; Commentary:
;;
;; Whirligig Functions
;;
;; Whirligigs return problems. Whirligigs return the next problem
;; in accordance with the player profile.
;;
;; Whirligigs are generators. The whirligig hangar keeps track of
;; existing whirligigs and provides access through tags
;;
;;; Code:

(define (make-whirligig gset-tag gmodule-variable)
  "Returns a whirligig, an engine capable of cycling through the
problems grouped under a tag, independently of other whirligig
instances, when called."
  (let* ((problems (get-tag-problems gset-tag gmodule-variable))
	 (num-of-problems (length problems))
	 (tag gset-tag))
    (lambda (msg)
      "Returns either the tag this whirligig is for (if MSG is 'tag),
or the problem that should next be queried (if MSG is a
gset-blob-counter."
      (if (eqv? msg 'tag)
	  tag
	  (list-ref problems
		    (modulo msg num-of-problems))))))

(define hangar
  (let ([whirligig-list '()])
    (lambda (lowest-scoring-gset-blob gmodule-id)
      "Creates new whirligigs as needed or uses them to retrieve the
next problem for a gset tag in a module."
      (let ((gset-tag (gset-blob-tag lowest-scoring-gset-blob))
	    (gset-blob-counter (gset-blob-counter
				lowest-scoring-gset-blob)))

	(define (helper remaining-whirligigs gmodule-object)
	  (cond ((null? remaining-whirligigs)
		 (set! whirligig-list (cons
				       (make-whirligig gset-tag
						       gmodule-object)
				       whirligig-list))
		 ((car whirligig-list) gset-blob-counter))
		((eqv? ((car remaining-whirligigs) 'tag) gset-tag)
		 ((car remaining-whirligigs) gset-blob-counter))
		(else (helper (cdr remaining-whirligigs)
			      gmodule-object))))

	(helper whirligig-list (gmodule-id->gmodule-object gmodule-id))))))
