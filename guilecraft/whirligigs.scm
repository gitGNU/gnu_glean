;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft whirligigs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft gset-ops)
  #:use-module (guilecraft gmodule-manager)
  #:export (whirl_make-whirligig
	    whirl_hangar))

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

(define whirl_make-whirligig
  (lambda (gset-tag gmodule-variable)
    "Returns a whirligig, an engine capable of cycling through the
problems grouped under a tag, independently of other whirligig
instances, when called."
    (let ([counter 0]
	  [max-counter (length (gset_get-tag-problems gset-tag
						 gmodule-variable))]
	  [list (gset_get-tag-problems gset-tag gmodule-variable)]
	  [tag gset-tag])
      (lambda (message)
	(cond ((eq? message 'current)
	       (if (= counter 0)
		   (list-ref list (- max-counter 1))
		   (list-ref list (- counter 1))))
	      ((eq? message 'next)
	       (begin
		 (if (= counter max-counter)
		     (set! counter 0))
		 (set! counter (+ counter 1))
		 (list-ref list (- counter 1))))
	      ((eq? message 'tag)
	       tag)
	      (else #f))))))

(define whirl_hangar
  (let ([whirligig-list '()])
    (lambda (message gset-tag gmodule-id)
      "Creates new whirligigs as needed or uses them to retrieve the
next problem for a gset tag in a module."
      (define helper
	(lambda (message tmp-whirligig-list gset-tag gmodule-object)
	  (cond ((null? tmp-whirligig-list)
		 (begin
		   (set! whirligig-list (cons
					 (whirl_make-whirligig gset-tag
							       gmodule-object)
					 whirligig-list))
		   ((car whirligig-list) message)))
		((equal? ((car tmp-whirligig-list) 'tag) gset-tag)
		 ((car tmp-whirligig-list) message))
		(else (helper message
			      (cdr tmp-whirligig-list)
			      gset-tag
			      gmodule-object)))))

      (helper message whirligig-list gset-tag (gmodule-id->gmodule-object gmodule-id)))))
