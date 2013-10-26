;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft comtools)
  #:use-module (rnrs)
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
      (let ((input (read port)))
	(if (string? input)
	    (gc-string->symbol input)
	    input)))
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
      (robowrite (symbol->gc-string object))
      (robowrite object)))


;; FIXME: these functions are necessary because the scheme (read)
;; function has difficulty reading symbols from a socket that contain
;; certain special characters. Nonetheless, as it stands, the symbols
;; with these special characters will be altered during their
;; journey, which means that their symbol equality predicates
;; comparing the symbol before the journey and after the journey never
;; return true. It is not quite clear to me what causes this at this stage.
(define (symbol->gc-string symbol)
  "Return a string whose first word is :symbol: and whose second word
is SYMBOL.

This procedure is used because the (read) procedure has difficulty
reading symbols with certain characters in them, as tested by
quickcheck."
  (if  (symbol? symbol)
       (string-append ":symbol: " (symbol->string symbol))
       (assertion-violation
	'symbol->gc-string
	"SYMBOL is not actually a symbol!"
	symbol)))

(define (gc-string->symbol string)
  "Return a symbol whose from STRING, if the string is a
gc-string. Otherwise, return STRING.

This procedure is used because the (read) procedure has difficulty
reading symbols with certain characters in them, as tested by
quickcheck."
  (if (and (string? string))
      (if (and (< 9 (string-length string))
	       (string=? (string-take string 9) ":symbol: "))
	  (string->symbol (string-drop string 9))
	  string)
      (assertion-violation
       'gc-strinng->symbol
       "STRING is not actually a string!"
       string)))
;;End

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
    (let ((value (accessor record)))
      (if (symbol? value)
	  (symbol->gc-string value)
	  value)))

  (if (record? record)
      (let ((rtd (record-rtd record)))
	(cons (symbol->gc-string (record-type-name rtd))
	      (map get-field-value (get-accessors rtd))))
      (assertion-violation
       'record->list
       "RECORD is not actually a record!"
       record)))

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
	((symbol? object)
	 (if (known-rc? object)
	     object
	     (symbol->gc-string object)))
	(else object)))

(define (list->record object)
  "Return a record, assembled from the pieces of the tagged list
OBJECT.

Return OBJECT if OBJECT is not a list."
  (if (list? object)
      (let* ([rn (car object)]		; Record Name
	     [rv (cdr object)])		; Record Values
	(if (string? rn)
	    (let ((rn (gc-string->symbol rn)))
	      (if (known-rc? rn)
		  (apply (get-rc rn)
			 (map (lambda (rvalue)
				(if (string? rvalue)
				    (gc-string->symbol rvalue)
				    rvalue)) rv))
		  #f))
	    (assertion-violation
	     'list->record
	     "RN is not a string. A symbol?"
	     object (car object))))
      object))

(define (list->record* object)
  "Return a record for every tagged list within OBJECT, and a list for
every normal list.

Return #f if object is not a list."
  (define (p->r* p)
    (let ((first (car p))
	  (rest (cdr p)))
      (cond ((list? first)
	     (cond ((list? rest)
		    (cons (l->r* first)
			  (l->r* rest)))
		   ((pair? rest)
		    (cons (l->r* first)
			  (p->r* rest)))
		   ((string? rest)
		    (cons (l->r* first)
			  (gc-string->symbol rest)))
		   (else (cons (l->r* first)
			       rest))))
	    ((pair? first)
	     (cond ((list? rest)
		    (cons (p->r* first)
			  (l->r* rest)))
		   ((pair? rest)
		    (cons (p->r* first)
			  (p->r* rest)))
		   ((string? rest)
		    (cons (p->r* first)
			  (gc-string->symbol rest)))
		   (else (cons (p->r* first)
			       rest))))
	    ((string? first)
	     (cond ((list? rest)
		    (cons (gc-string->symbol first)
			  (l->r* rest)))
		   ((pair? rest)
		    (cons (gc-string->symbol first)
			  (p->r* rest)))
		   ((string? rest)
		    (cons (gc-string->symbol first)
			  (gc-string->symbol rest)))
		   (else (cons (p->r* first)
			       rest))))
	    (else (cond ((list? rest)
			 (cons first
			       (l->r* rest)))
			((pair? rest)
			 (cons first
			       (p->r* rest)))
			((string? rest)
			 (cons first
			       (gc-string->symbol rest)))
			(else p))))))
  (define (l->r* l)			; recurse through l
    (if (null? l)			; If l is null then we're done
	'()
	(let ((first (car l))
	      (rest (cdr l)))
	  (cond ((list? first)		; Recurse on car & cdr?
		 (cons (l->r* first)
		       (l->r* rest)))
		((pair? first)
		 (cons (p->r* first)
		       (l->r* rest)))
		((string? first)
		 (if (known-rc? (gc-string->symbol first)) ; embedded tagged list?
		     (list->record
		      (cons first
			    (l->r* rest)))
		     (cons (gc-string->symbol first)
			   (l->r* rest)))) ; recurse only on cdr
		(else
		 (cons first
		       (l->r* rest))))))) ; recurse only on cdr)

  (cond ((list? object)
	 (l->r* object))		; analyse tlist
	((pair? object)
	 (p->r* object))
	(else #f)))			; return false if not a list
