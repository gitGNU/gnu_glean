;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

;;;; Sets

;;; copyright etc. & license.

;;; Sets are recursive records used to group problems and other sets
;;; together for use in Guilecraft.
;;;
;;; Sets are a means of grouping problems together under a
;;; tag. Guilecraft uses the tag to measure progress against a
;;; discipline, and will select a "random" question from the
;;; algorithmically determined highest priority set.
;;;
;;; Alternatively, sets can contain other sets — a way to group sets
;;; of problems in meta-groups: modules if you will.

;;;; Documentation
;;;
;;; (SET id
;;;      [information]
;;;      [problem …]
;;;      [set …]) -> set-type
;;; (SET? <object>) -> boolean
;;; (SET-ID <set>) -> set-id
;;; (SET-INFO <set>) -> set-information
;;; (SET-)

;;;;; Set Operations

;;;;; Primitive Operations

;;;;; Module Definition
(define-module (guilecraft data-types sets)
;;  #:use-module (srfi srfi-9)
  #:use-module (rnrs records syntactic)
  #:export (
	    set
	    set?
	    set-id
	    set-contents
	    set-info
	    set-name
	    set-version
	    set-synopsis
	    set-description
	    set-creator
	    set-attribution
	    set-resources

	    problem
	    problem?
	    problem-q
	    problem-s
	    problem-o
	    problem-p

	    q
	    q?
	    q-text
	    q-media

	    s
	    s?
	    s-text
	    s-media

	    o
	    o?
	    o-text
	    o-media

	    media
	    media?
	    media-urls
	    media-books
	    media-images
	    media-videos
	    media-audio
	    ))

;;;;; Set Structure

(define-record-type (gset set set?)
  (fields (immutable id set-id)
	  (immutable contents set-contents)
	  (immutable name set-name)
	  (immutable version set-version)
	  (immutable synopsis set-synopsis)
	  (immutable description set-description)
	  (immutable creator set-creator)
	  (immutable attribution set-attribution)
	  (immutable resources set-resources))
  (protocol
   (lambda (new)
     (lambda* (id #:key (contents #f) (name #f) (version #f)
		  (synopsis #f) (description #f) (creator #f)
		  (attribution #f) (resources #f))

	      (new id 
		   (if (list? contents) contents (list contents))
		   name
		   version
		   synopsis
		   description
		   creator
		   (if (list? attribution)
		       attribution
		       (list attribution))
		   (if (list? resources)
		       resources
		       (list resources)))))))

(define-record-type (prob problem problem?)
  (fields (immutable q problem-q)
	  (immutable s problem-s)
	  (immutable o problem-o)
	  (immutable p problem-p))
  (protocol
   (lambda (new)
     (lambda (q s . args)

       (define (parse-args options predicate remaining)
	 (cond ((null? remaining)
		(list options predicate))
	       ((list? (car remaining))
		(parse-args (append options (car remaining))
			    predicate
			    (cdr remaining)))
	       ((procedure? (car remaining))
		(parse-args options
			    (car remaining)
			    (cdr remaining)))))

       (apply new q s (parse-args '() 'equal? args))))))

(define-record-type (question q q?)
  (fields (immutable text q-text)
	  (immutable media q-media))
  (protocol
   (lambda (new)
     (lambda* (text #:key (media #f))
	      (new text media)))))

(define-record-type (solution s s?)
  (fields (immutable text s-text)
	  (immutable media s-media))
  (protocol
   (lambda (new)
     (lambda* (text #:key (media #f))
	      (new text media)))))

(define-record-type (option o o?)
  (fields (immutable text o-text)
	  (immutable media o-media))
  (protocol
   (lambda (new)
     (lambda* (text #:key (media #f))
	      (new text media)))))

(define-record-type (medium media media?)
  (fields (immutable urls media-urls)
	  (immutable books media-books)
	  (immutable images media-images)
	  (immutable videos media-videos)
	  (immutable audio media-audio))
  (protocol
   (lambda (new)
     (lambda* (#:key (urls #f) (books #f) (images #f) (videos #f)
		     (audio #f))
	      (new urls books images videos audio)))))
