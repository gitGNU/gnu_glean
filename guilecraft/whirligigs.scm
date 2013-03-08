;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft whirligigs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (guilecraft gsets)
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
    (lambda (message gset-tag gmodule-variable)
      "Creates new whirligigs as needed or uses them to retrieve the
next question/answer for a gset tag in a module."
      (define helper
	(lambda (tmp-whirligig-list message gset-tag gmodule-variable)
	  (cond ((null? tmp-whirligig-list)
		 (begin
		   (set! whirligig-list (cons (whirl_make-whirligig gset-tag
							      gmodule-variable)
					      whirligig-list))
		   ((car whirligig-list) message)))
		((equal? ((car tmp-whirligig-list) 'tag) gset-tag)
		 ((car tmp-whirligig-list) message))
		(else (helper (cdr tmp-whirligig-list) 
			      message
			      gset-tag 
			      gmodule-variable)))))

      (helper whirligig-list message gset-tag gmodule-variable))))
