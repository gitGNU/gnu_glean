;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft whirligigs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft gsets)
  #:use-module (guilecraft gmodules)
  #:use-module (guilecraft data-manager)
  #:export (;; temp helper functions
	    whirl_make-whirligig 
	    whirl_get-next-problem 
	    whirl_get-current-problem 
	    whirl_hangar
	    whirl_issue-challenge))

;;; 
;; Whirligig Functions
;;; 

;; Whirligigs are generators. The whirligig hangar keeps track of
;; existing whirligigs and provides access through tags
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
next question/answer for a gset tag in a module."
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
		(else (helper (cdr tmp-whirligig-list) 
			      message
			      gset-tag 
			      gmodule-object)))))

      (helper message whirligig-list gset-tag (gmodule-id->gmodule-object gmodule-id)))))


;;; For now transmute-gmodule-id is hardcoded to check some known
;;; gmodules. In future it needs to scan the gmodule directory
;;; dynamically when transmute-gmodule-id first encounters a new
;;; gmodule-id. 
(define gmodule-id->gmodule-object
  (lambda (gmodule-id)
    "Return the gmodule that is named by gmodule-id."
    (dman_get-gmodule gmodule-id)))

