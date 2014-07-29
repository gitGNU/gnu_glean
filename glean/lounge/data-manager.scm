;;; glean --- fast learning tool.         -*- coding: utf-8 -*-

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
;; keep track of, and make use of the gmodules known to glean;
;; and by problem-type-manager, to keep track of, and make use of the
;; problem-types (the templates for problems used in gmodules) known
;; to glean.
;;
;; Code:

(define-module (glean lounge data-manager)
  #:use-module (glean common utils)
  #:use-module (rnrs)
  #:export (data-manager))

(define data-manager
  (lambda (predicate)
    "Uniform non-persistent data-storage for Glean implemented as
a hash-table.

Currently used by: the module store, the profile store."

    (let ([data (make-eqv-hashtable)])
      (lambda (message . args)
	"Return the value associated with (car ARGS) if MESSAGE is
'get, or #f if the (car ARGS) is not a key in the hashtable.

 Return #t if MESSAGE is 'put, and (cadr ARGS) passes the predicate
check and is added to table. Return #f otherwise.

Return all values stored in the table when MESSAGE is 'list."

	(cond ((eqv? message 'get)      ; Retrieving an entry
	       (hashtable-ref data (car args) #f))
	      ((eqv? message 'put)	; Storing an entry
	       (if (predicate (cadr args))
		   (begin
		     (hashtable-set! data (car args) (cadr args))
		     #t)
		   #f))
              ((eqv? message 'rem-val)
               (call-with-values
                   ;; fetch values and keys
                   (lambda () (hashtable-entries data))
                 (lambda (ks vs)
                   (vector-for-each
                    ;; destroy every entry who's value matches arg to
                    ;; 'rem-val
                    (lambda (k v)
                      (if (equal? v (car args))
                          (hashtable-delete! data k)))
                    ks vs))))
	      ((eqv? message 'rem)
	       (hashtable-delete! data (car args)))
	      ((eqv? message 'list-keys (hashtable-keys data)))
	      ((eqv? message 'list) (hashtable-entries data))
	      ((eqv? message 'values)
	       (vector->list
		(vector-map (lambda (key)
			      (hashtable-ref data key #f))
			    (hashtable-keys data))))
	      ((eqv? message 'contains)
	       (hashtable-contains? data (car args)))
	      ((eqv? message 'update)
	       (hashtable-update! data (car args)
				  (cadr args) (caddr args)))
	      (else (error "data-manager: unknown message: "
			   message)))))))
