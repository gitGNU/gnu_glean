;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft comtools)
  #:use-module (rnrs records procedural)
  #:use-module (rnrs records inspection)
  #:use-module (guilecraft config)
  #:use-module (guilecraft gprofile-ops)
  #:use-module (guilecraft gmodule-ops)
  #:use-module (guilecraft utils)
  #:use-module (guilecraft record-index)
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

(define (record->list record)
  "Return a list consing RECORD's field-values together, and prepend
with RECORD's record-type-descriptor."
  (define (get-accessors rtd)
    (define (accessors field-ref collected)
      (let ((collected (cons (record-accessor rtd field-ref)
			     collected)))
	(cond ((zero? field-ref)
	       collected)
	      (else (accessors (1- field-ref) collected)))))
    (let ((nr-fields (vector-length (record-type-field-names rtd))))
      (if (zero? nr-fields)
	  '()
	  (accessors (1- nr-fields) '()))))
  (define (get-field-value accessor)
    (accessor record))

  (let ((rtd (record-rtd record)))
    (cons (record-type-name rtd)
	  (map get-field-value (get-accessors rtd)))))

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
  (if (list? object)
      (let* ([rn (car object)]		; Record Name
	     [rv (cdr object)])		; Record Values
	(if (known-rc? rn)
	    (apply (get-rc rn) rv)
	    #f))
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
	  ((known-rc? (car l))		; embedded tagged list?
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
