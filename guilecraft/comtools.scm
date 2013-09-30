;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft comtools)
  #:use-module (guilecraft config)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft gmodule-ops)
  #:use-module (guilecraft utils)
  #:use-module (guilecraft known-rtd-manager)
  #:use-module (guilecraft data-types requests)
  
  #:export (record->list
	    record->list*
	    list->record
	    list->record*
	    gwrite
	    gread
	    alive?
	    server
	    exchange))

(define (server)
  (let* ((s (socket PF_UNIX SOCK_STREAM 0))
	 (path (string-append %guilecraft-dir% "/socket"))
	 (address (make-socket-address AF_UNIX path)))
    (connect s address)
    s))

(define (exchange request)
  "Connect to server, send request, return response or #f if
connection fails."
  (let ((s (server)))
    (gwrite (record->list* request) s)
    (let ((result (list->record* (gread s))))
      (close s)
      result)))

(define (alive?)
  "Check whether a Guilecraft server exists. Return #t, otherwise #f.

PATH is the path to the unix domain socket that will be tested for
server existence."
  (catch #t (lambda ()
	      (and (access? %guilecraft-socket% W_OK)
		   (exchange (request (alive-rq)))))
    (lambda (key . args)
      key args				; ignored
      #f)))

(define (gread port)
  "Provide means to try to read from port; return error on failure."
  (catch #t
    (lambda ()
      (read port))
    (lambda (k . args)
      (gmsg "gread: Port closed prematurely: " k args)
      #f)))

(define (gwrite object port)
  "Provide means to write symbols and return values to port, as well
as scheme objects."
  (define (robowrite o)
    (catch #t
      (lambda ()
	(gmsg "gwrite: attempting to write")
	(write o port)
	(gmsg "gwrite: written")
	#t)
      (lambda (k . args)
	(gmsg "gwrite: Port closed prematurely: " k args)
	#f)))
  (if (symbol? object)
      (robowrite (list object))
      (robowrite object)))

;; Turn an arbitrary record into a tagged-list (tl).
(define (record->list record)
  "Return a list consing RECORD's field-values together, and prepend
with RECORD's record-type-descriptor."
  (define (rec-field-values ras)
    "Return the values of the different fields in RECORD, by using the
list of record-accessors RAS."
    (define (rfv ra)
      "Return the value of record-field accessed through
record-accessor ra."
      (ra record))
    (map rfv ras))
  (define (rec-accessors rtd)
    "Return a list of record-accessors for the record of type RTD."
    (define (this-ra)
      "Return a procedure that, given a record-type-field, would
return the record accessor for a record of type RTD."
      (lambda (record-type-field)
	(record-accessor rtd record-type-field)))
    (map (this-ra) (record-type-fields rtd)))

  (let* ((rtd (record-type-descriptor record))
	 (rtn (record-type-name rtd)))
    (begin (known-rtds 'put rtn rtd)  ; Side-effect: update known-rtds
	   (cons rtn		      ; tlist: rtn + rtvs
		 (rec-field-values (rec-accessors rtd))))))

;; Recurse through a record, turning each contained record into a list
;; to pump it through sockets.
;; object could be a list, atom or record.
(define (record->list* object)
  "Return a list if OBJECT is a record. Then recurse through the
generated list to reduce further records into lists."
  (cond ((or (list? object)
	     (pair? object))
	 (if (null? object)
	     object
	     (cons (record->list* (car object))
		   (record->list* (cdr object)))))
	((record? object)
	 (record->list*
	  (record->list object)))
	(else object)))

(define (list->record object)
  "Return a record, assembled from the pieces of the tagged list
OBJECT.

Return OBJECT if OBJECT is not a list."
  ;; Normally the record-constructor is derived from known-rtds, but the
  ;; define-record-type* procedure used for gmodules and gprofiles in
  ;; particular do not allow for this (record-constructor does not work
  ;; for them). As a result I've added clauses for these data-types
  ;; for now. Instead of this hack, the define-record-type* macro
  ;; should be expanded to handle record-constructor gracefully.
  (if (list? object)
      (let ([rn (car object)]		; Record-Type-Name
	    [rv (cdr object)])		; Record-Type-Values
	(cond ((eqv? rn '<profile>)
	       (apply trad-make-profile rv)) ; Profiles hack
	      ((eqv? rn '<gmodule>)
	       (apply trad-make-gmodule rv)) ; Gmodules hack
	      (else
	       (apply (record-constructor (known-rtds 'get rn)) rv))))
      object))

(define (list->record* object)
  "Return a record for every tagged list within OBJECT, and a list for
every normal list.

Return #f if object is not a list."
  (define (p->r* p)
    (cond ((list? (car p))
	   (cond ((list? (cdr p))
		  (cons (l->r* (car p))
			(l->r* (cdr p))))
		 ((pair? (cdr p))
		  (cons (l->r* (car p))
			(p->r* (cdr p))))
		 (else (cons (l->r* (car p))
			     (cdr p)))))
	  ((pair? (car p))
	   (cond ((list? (cdr p))
		  (cons (p->r* (car p))
			(l->r* (cdr p))))
		 ((pair? (cdr p))
		  (cons (p->r* (car p))
			(p->r* (cdr p))))
		 (else (cons (p->r* (car p))
			     (cdr p)))))
	  (else (cond ((list? (cdr p))
		       (cons (car p)
			     (l->r* (cdr p))))
		      ((pair? (cdr p))
		       (cons (car p)
			     (p->r* (cdr p))))
		      (else p)))))
  (define (l->r* l)			; recurse through l
    (cond ((null? l) '())		; If l is empty, we're done
	  ((list? (car l))		; Recurse on car & cdr?
	   (cons (l->r* (car l))
		 (l->r* (cdr l))))
	  ((pair? (car l))
	   (cons (p->r* (car l))
		 (l->r* (cdr l))))
	  ((known-rtd? (car l))		; embedded tagged list?
	   (list->record
	    (cons (car l)
		  (l->r* (cdr l)))))	; recurse only on cdr
	  (else
	   (cons (car l)
		 (l->r* (cdr l))))))	; recurse only on cdr
  (cond ((list? object)
	 (l->r* object))		; analyse tlist
	((pair? object)
	 (p->r* object))
	(else #f)))			; return false if not a list
