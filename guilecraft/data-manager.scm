;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;; Commentary:
;;
;; Gmodule-Manager defines an interface to centrally store all known
;; gmodules. The table is indexed on gmodule-id and it returns a
;; gmodule object.
;; Gmodule-Manager also defines an interface 

(define-module (guilecraft data-manager)
  #:use-module (guilecraft gmodules)
  #:export (dman_add-gmodule
	    dman_get-gmodule
	    dman_gmodule-manager))

(define first-arg
  (lambda (args)
    (car args)))
(define first-tag
  (lambda (data-type-table)
    (car (car data-type-table))))
(define first-data-type-object
  (lambda (data-type-table)
    (cdr (car data-type-table))))
(define rest-of-table
  (lambda (data-type-table)
    (cdr data-type-table)))

(define dman_data-manager
  (lambda ()
    "Generic data-managing table interface. Intended as a factory to
spawn tables to handle specific data-types (e.g. gmodules or
problem-types)."
    (let ([data-type-table '()])
      (lambda (message . args)
	"Takes 'put or 'get as message, which either installs a
data-type (e.g. gmodule, problem-type) into the table or returns a
data-type from the table: 
dman_data-manager 'put data-type -> appends data-type object,
indexed by its data-type-id to the data-type-table and returns 'done or
#f. 
dman_data-manager 'get data-type-id -> returns a data-type object
associated with data-type-id or #f if no data-type object can be found.

data-type-table is the central, canonical repository of known data-types. All
data-types should implement a call to their respective data-manager's
'put function, so that they can be added to the that manager on load."
	(define data-type-getter
	  (lambda (arg tmp-data-type-table)
	    "Return data-type-object if arg is found in tmp-data-type-table and #f
if not. Recurses through tmp-data-type-table."
	    (cond ((null? tmp-data-type-table)
		   #f)
		  ((eq? arg (first-tag tmp-data-type-table))
		   (first-data-type-object tmp-data-type-table))
		  (else (data-type-getter arg (rest-of-table
					       tmp-data-type-table))))))

	(define data-type-putter
	  (lambda (data-type-object data-type-table)
	    "Returns a new data-type-table with the new data-type-object added."
	    (cons (cons (gmod_get-id data-type-object) data-type-object) data-type-table)))

	(cond ((eq? message 'get)
	       (data-type-getter (first-arg args) data-type-table))
	      ((eq? message 'put)
	       (begin
		 (set! data-type-table (data-type-putter (first-arg args) data-type-table))
		 #t))
	      ((eq? message 'list)
	       data-type-table)
	      (else (error "data-type-manager: unknown message: " message)))))))

(define dman_add-gmodule
  (lambda (gmodule-object)
    "Convenience procedure to add a given gmodule to the
gmodule-manager."
    (dman_gmodule-manager 'put gmodule-object)))

(define dman_get-gmodule
  (lambda (gmodule-tag)
    "Convenience procedure to retrieve a given gmodule from the active
gmodule-table."
    (dman_gmodule-manager 'get gmodule-tag)))

(define dman_gmodule-manager (dman_data-manager))
