;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;; Commentary:
;;
;; data-manager provides the dman_make-data-manager procedure, which,
;; much like make-whirligig, supplies a procedure with local state to
;; track dynamic server side content.
;; The procedure returned by make-data-manager maintains a table,
;; indexed by an arbitrary identifier, retrieved through the procedure
;; @var{identifier-getter}, passed to make-data-manager when called.
;; Once created, the new data-manager accepts 'put, get and 'list as
;; messages. 'put expects a data-type provider and a data-type
;; prototype. An identifier is extracted from data-type prototype
;; using identifier-getter, after which, each time the data-manager
;; procedure is called with 'get data-type object, the data-type
;; provider is used to manipulate it.
;;
;; Currently data-manager procedures are used by gmodule-manager, to
;; keep track of, and make use of the gmodules known to guilecraft;
;; and by problem-type-manager, to keep track of, and make use of the
;; problem-types (the templates for problems used in gmodules) known
;; to guilecraft.
;;
;; Code:

(define-module (guilecraft data-manager)
  #:use-module (guilecraft gmodules)

  #:export (dman_add-gmodule
	    dman_get-gmodule
	    dman_data-manager))

(define dman_data-manager
  (lambda (identifier-getter)
    "Generic data-managing table interface. Intended as a factory to
spawn tables to handle specific data-types (e.g. gmodules or
problem-types).

identifier-getter must be a procedure that, when applied to a
data-type object managed by the data-manager instance, returns the
object's identifier (e.g. its type for problem-types or ID tag for
gmodules)."

    (let ([data-type-table '()])
      (lambda (message . args)
	"Takes 'put, 'get or 'list as message. Either (with 'put
data-object-prototype) installs a data-type (e.g. gmodule,
problem-type) into the table, or, when called with 'get and an indexed
data-type identifier returns content or a content provider for that
data-type:
dman_data-manager 'put data-type -> appends data-type object,
indexed by its data-type-id to the data-type-table and returns 'done or
#f. 
dman_data-manager 'get data-type-id -> returns a data-type object
associated with data-type-id or #f if no data-type object can be found.

data-type-table is the central, canonical repository of known data-types. All
data-types should implement a call to their respective data-manager's
'put function, so that they can be added to the that manager on load."
	(define first-id
	  (lambda (data-type-table)
	    (car (car data-type-table))))
	(define first-data-type-object
	  (lambda (data-type-table)
	    (cdr (car data-type-table))))
	(define rest-of-table
	  (lambda (data-type-table)
	    (cdr data-type-table)))

	(define data-type-getter
	  (lambda (id tmp-data-type-table)
	    "Return content associated with id from the table, if id
is an index in the table. Return #f otherwise."
	    (cond ((null? tmp-data-type-table)
		   #f)
		  ((eq? id (first-id tmp-data-type-table))
		   (first-data-type-object tmp-data-type-table))
		  (else (data-type-getter id (rest-of-table
					       tmp-data-type-table))))))

	(define data-type-putter
	  (lambda (args identifier-getter old-data-type-table)
	    "Returns a new table with the new content
(data-type-interface) added by index id, retrieved through applying
identifier-getter to the data-type-prototype."
	    (let ([data-interface (car args)] [data-prototype (cadr args)])
	      (cons (cons (identifier-getter data-prototype) data-interface) old-data-type-table))))


	(cond ((eq? message 'get)
	       (let ([result (data-type-getter (car args)
					       data-type-table)])
		 (if result
		     result
		     (error "data-manager 'get: index not found: "
			    (car args)))))
	      ((eq? message 'put)
	       (begin
		 (set! data-type-table (data-type-putter args identifier-getter data-type-table))
		 #t))
	      ((eq? message 'list)
	       data-type-table)

	      (else (error "data-manager: unknown message: " message)))))))

(define dman_add-gmodule
  (lambda (gmodule-object)
    "Convenience procedure to add a given gmodule to the
gmodule-manager.

gmodule-manager is an instance of data-manager. It stores the
gmodule-object indexed by its gmodule-id, derived using gmod_get-id."
    (gmodule-manager 'put gmodule-object gmodule-object)))

(define dman_get-gmodule
  (lambda (gmodule-id)
    "Convenience procedure to retrieve a given gmodule from the active
gmodule-table stored in gmodule-manager."
    (gmodule-manager 'get gmodule-id)))

; define a data-manager instance using the result of gmod_get-id as key
(define gmodule-manager (dman_data-manager gmod_get-id))
