;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;; Commentary:
;;
;; Gmodule-Manager defines an interface to centrally store all known
;; gmodules. The table is indexed on gmodule-id and it returns a
;; gmodule object.
;; Gmodule-Manager also defines an interface 

(define-module (guilecraft gmodule-manager)
  #:use-module (guilecraft gmodules)
  #:export (gman_add-gmodule
	    gman_get-gmodule
	    gman_gmodule-manager))

(define first-arg
  (lambda (args)
    (car args)))
(define first-tag
  (lambda (gmodule-table)
    (car (car gmodule-table))))
(define first-gmodule-object
  (lambda (gmodule-table)
    (cdr (car gmodule-table))))
(define rest-of-table
  (lambda (gmodule-table)
    (cdr gmodule-table)))

(define gman_add-gmodule
  (lambda (gmodule-object)
    "Convenience procedure to add a given gmodule to the
gmodule-manager."
    (gman_gmodule-manager 'put gmodule-object)))

(define gman_get-gmodule
  (lambda (gmodule-tag)
    "Convenience procedure to retrieve a given gmodule from the active
gmodule-table."
    (gman_gmodule-manager 'get gmodule-tag)))

(define gman_gmodule-manager
  (let ([gmodule-table '()])
    (lambda (message . args)
      "Takes 'put or 'get as message, which either installs a gmodule
into the table or returns a gmodule from the table:
gmodule-manager 'put gmodule -> appends gmodule a gmodule object,
indexed by its gmodule-id to the gmodule-table and returns 'done or
#f. 
gmodule-manager 'get gmodule-id -> returns a gmodule object
associated with gmodule-id or #f if no gmodule object can be found.

gmodule-table is the central repository of known gmodules. All
gmodules should implement a gmodule-manager 'put function, so that
they can be added to the manager on load."
      (cond ((eq? message 'get)
	     (gmodule-getter (first-arg args) gmodule-table))
	    ((eq? message 'put)
	     (begin
	       (set! gmodule-table (gmodule-putter (first-arg args) gmodule-table))
	       #t))
	    ((eq? message 'list)
	     gmodule-table)
	    (else (error "gmodule-manager: unknown message: " message))))))


(define gmodule-getter
  (lambda (arg tmp-gmodule-table)
    "Return gmodule-object if arg is found in tmp-gmodule-table and #f
if not. Recurses through tmp-gmodule-table."
    (cond ((null? tmp-gmodule-table)
	   #f)
	  ((eq? arg (first-tag tmp-gmodule-table))
	   (first-gmodule-object tmp-gmodule-table))
	  (else (gmodule-getter arg (rest-of-table
				     tmp-gmodule-table))))))

(define gmodule-putter
  (lambda (gmodule-object gmodule-table)
    "Returns a new gmodule-table with the new gmodule-object added."
    (cons (cons (gmod_get-id gmodule-object) gmodule-object) gmodule-table)))
