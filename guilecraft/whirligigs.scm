;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft whirligigs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft gset-ops)
  #:use-module (guilecraft utils)
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

(define (make-whirligig gset-tag gmodule-object)
  "Returns a whirligig, an engine capable of cycling through the
problems grouped under a tag, independently of other whirligig
instances, when called."
  (if (or (not gset-tag)
	  (not (set? gmodule-object)))
      (assertion-violation
       'make-whirligig
       "&irritants are not a gset-tag or gmodule-object."
       gset-tag gmodule-object)
      (let* ((problems (get-tag-problems gset-tag gmodule-object))
	     (num-of-problems (length problems))
	     (tag gset-tag))
	(lambda (msg)
	  "Returns either the tag this whirligig is for (if MSG is 'tag),
or the problem that should next be queried (if MSG is a
set-blob-counter."
	  (if (eqv? msg 'tag)
	      tag
	      (list-ref problems
			(modulo msg num-of-problems)))))))

;; FIXME: It would be cleaner if we passed gset-object instead of
;; gset-tag here: we could remove the need to pass gmodule-object as
;; well as gset-tag.
(define hangar
  (let ([whirligig-list '()])
    (lambda (module-id lowest-scoring-set-blob set-id)
      "Creates new whirligigs as needed or uses them to retrieve the
next problem for a gset tag in a module."
      (let ((gset-tag (set-blob-id lowest-scoring-set-blob))
	    (counter (set-blob-counter lowest-scoring-set-blob)))

	(define (helper remaining-whirligigs gmodule-object)
	  (cond ((null? remaining-whirligigs)
		 (set! whirligig-list (cons
				       (make-whirligig gset-tag
						       gmodule-object)
				       whirligig-list))
		 ((car whirligig-list) counter))
		((eqv? ((car remaining-whirligigs) 'tag) gset-tag)
		 ((car remaining-whirligigs) counter))
		(else (helper (cdr remaining-whirligigs)
			      gmodule-object))))
	(helper whirligig-list (set-id->gmodule-object module-id))))))
