;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft utils)
  #:use-module (guilecraft config)
  #:use-module (rnrs)
  #:export (flatten
	    clog
	    llog
	    gmsg
	    rprinter))

(define (flatten obj)
  (cond ((null? obj) '())
	((not (pair? obj)) (list obj))
	(else
	 (append (flatten (car obj))
		 (flatten (cdr obj))))))

(define (llog args)
  "Returns #undefined. Provide logic-warning log abstraction:
a gmsg call with ARGS defaulting to #:priority 1."
  (gmsg #:priority 1 args))

(define (clog args)
  "Returns #undefined. Provide communications-warning log abstraction:
a gmsg call with ARGS defaulting to #:priority 3."
  (gmsg #:priority 3 args))

(define (rprinter record)
  "Returns #f if RECORD is not a record, #t otherwise. Recursively
prints RECORD and its fields as a side-effect if #t."

  (if (record? record)
      (let* ((rtd (record-rtd record))
	     (fields (vector->list (record-type-field-names rtd)))
	     (length (length fields)))

	(define (print-fields remaining index)
	  (cond ((null? remaining)
		 #t)
		(else
		 (let ((field-value ((record-accessor rtd index)
				     record)))
		   (cond ((record? field-value)
			  (simple-format #t "   ~a: ~a" (car remaining)
					 field-value)
			  (newline)
			  (rprinter field-value))
			 ((and (list? field-value)
			       (not (null? field-value))
			       (record? (car field-value)))
			  (for-each (lambda (arg)
				      (rprinter arg))
				    field-value))
			 (else
			  (simple-format #t "   ~a: ~a" (car remaining)
					 field-value)
			  (newline))))
		 (print-fields (cdr remaining) (1+ index)))))

	(simple-format #t "record: ~a" (record-type-name rtd))
	(newline)
	(print-fields fields 0)
	(simple-format #t ":end:")
	(newline))
      #f))

(define* (gmsg #:key (priority 10) . args)
  "Provide a simple debugging message system. Prints ARGS with
Simple-Format if PRIORITY is lower than %DEBUG% set in config.scm."
  (define (indent length)
    (define (i l s)
      (if (zero? l)
	  s
	  (i (1- l) (string-append " " s))))
    
    (if (zero? length)
	""
	(i (1- length) " ")))
  (define (gm args format-string)
    (cond ((null? args) format-string)
	  (else (gm (cdr args) (string-append format-string " ~S")))))
  (if (< priority %debug%)
      (begin
	(simple-format #t "~a* Debug:" (indent (1- priority)))
	(apply simple-format #t (gm args " ") args)
	(newline))))
