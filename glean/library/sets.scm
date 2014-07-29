;;; glean --- Fast learning tool.         -*- coding: utf-8 -*-

;;;; Sets

;; Copyright (C) 2008, 2010, 2012 Alex Sassmannshausen

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:

;;; Sets are recursive records used to group problems and other sets
;;; together for use in Glean.
;;;
;;; Sets are a means of grouping problems together under a
;;; tag. Glean uses the tag to measure progress against a
;;; discipline, and will select a "random" question from the
;;; algorithmically determined highest priority set.
;;;
;;; Alternatively, sets can contain other sets — a way to group sets
;;; of problems in meta-groups: modules if you will.
;;;
;;; This module also defines any sub-records used in set/module
;;; creation, and as such is glean's primary data-type library.
;;;
;;; Finally this library provides an embedded means of content
;;; definition error detection, and a means to provide meaningful help
;;; to content creators on the back of this.

;;;; Documentation
;;;  (Incomplete as this may still change)
;;;
;;; (SET id
;;;      [information]
;;;      [problem …]
;;;      [set …]) -> set-type
;;; (SET? <object>) -> boolean
;;; (SET-ID <set>) -> set-id
;;; (SET-INFO <set>) -> set-information
;;; (SET-)
;;;
;;;; Tests:
;;; (tests sets)
;;;
;;; Code:

;;;;; Module Definition
(define-module (glean library sets)
  #:use-module (glean common utils)
  #:use-module (ice-9 match)
  #:use-module (rnrs records syntactic)
  #:use-module (rnrs records procedural)
  #:use-module (srfi srfi-1)
  #:export (
	    set
            set?
            rootset?
            module
            module?
            tutorial
            tutorial?
            number-of-problems
	    
	    make-set
	    mecha-set
	    set-rcd
 	    set-id
	    set-contents
	    set-info
	    set-name
	    set-version
	    set-synopsis
	    set-description
            set-keywords
	    set-creator
	    set-attribution
	    set-resources
	    set-properties
            set-logo

	    problem
	    make-problem
	    problem?
	    problem-q
	    problem-s
	    problem-o
	    problem-p

	    q
	    make-q
	    q?
	    q-text
	    q-media

	    s
	    make-s
	    s?
	    s-text
	    s-media

	    o
	    make-o
	    o?
	    o-text
	    o-media

	    p
	    p?
	    p-function

	    media
	    make-media
	    media?
	    media-urls
	    media-books
	    media-images
	    media-videos
	    media-audio
	    ))

;;;;; Set Usage
;;;; This section details porcelain commands that should normally be
;;;; used by humans to generate sets and/or modules.
(define (number-of-problems set)
  (define (num-of-problems-helper set count)
    (if (rootset? set)
        (+ count (length (set-contents set)))
        (fold num-of-problems-helper count (set-contents set))))

  (if (set? set)
      (num-of-problems-helper set 0)))

(define* (module id #:key (contents '()) (name "") (version "")
	   (synopsis "") (description "") (keywords '())
           (creator "") (attribution '()) (resources '()) (logo "")
           (properties '()))
  "High level convenenience interface to make-set, for the
creation of modules."
  (make-set id contents name version synopsis description keywords
            creator attribution resources logo
            (if (assoc 'module properties)
                properties
                (acons 'module #t properties))))

(define* (set id #:key (contents '()) (name "") (version "")
              (synopsis "") (description "") (keywords '())
              (creator "") (attribution '()) (resources '())
              (logo "") (properties '()))
  "High level convenenience interface to make-set, for the
creation of sets."
  (make-set id contents name version synopsis description keywords
            creator attribution resources logo properties))

(define* (tutorial id #:key (contents '()) (name "") (version "")
                   (synopsis "") (description "") (keywords '())
                   (creator "") (attribution '()) (resources '())
                   (logo "") (properties '()))
  "High level convenenience interface to make-set, for the
creation of tutorials."
  (make-set id contents name version synopsis description keywords
            creator attribution resources logo
            (if (assoc 'tutorial properties)
                properties
                (acons 'tutorial #t properties))))

;;;;; Set Structure
;;;; The (rnrs records syntactic) record definitions for all things
;;;; related to glean module generation. The definitions
;;;; themselves are pretty standard, but all define protocol that uses
;;;; the validator procedure, which I hope is an effective way to make
;;;; help available to module and content maintainers (see the
;;;; relevant section below for details).

;;;;;; Questions
(define question-rtd
  (make-record-type-descriptor 'question #f #f #f #f
			       '#((immutable text)
				  (immutable media))))
(define question-rcd
  (make-record-constructor-descriptor
   question-rtd #f (lambda (new)
		     (lambda*
		      (text #:optional (media (media)))
		      (validator new
				 (list (vstring? 'err-q-text)
				       (vtype? media? 'err-q-media))
				 text media)))))
(define q (record-constructor question-rcd))
(define make-q (record-constructor
		(make-record-constructor-descriptor question-rtd #f #f)))
(define q? (record-predicate question-rtd))
(define q-text (record-accessor question-rtd 0))
(define q-media (record-accessor question-rtd 1))

;;;;;; Solutions
(define solution-rtd
  (make-record-type-descriptor 'solution #f #f #f #f
			       '#((immutable text)
				  (immutable media))))
(define solution-rcd
  (make-record-constructor-descriptor
   solution-rtd #f (lambda (new)
		     (lambda*
		      (text #:optional (media (media)))
		      (validator new
				 (list (vstring? 'err-s-text)
				       (vtype? media? 'err-s-media))
				 text media)))))
(define s (record-constructor solution-rcd))
(define make-s (record-constructor
		(make-record-constructor-descriptor solution-rtd #f #f)))
(define s? (record-predicate solution-rtd))
(define s-text (record-accessor solution-rtd 0))
(define s-media (record-accessor solution-rtd 1))

;;;;;; Options
(define option-rtd
  (make-record-type-descriptor 'option #f #f #f #f
			       '#((immutable text)
				  (immutable media))))
(define option-rcd
  (make-record-constructor-descriptor
   option-rtd #f (lambda (new)
		     (lambda*
		      (text #:optional (media (media)))
		      (validator new
				 (list (vstring? 'err-o-text)
				       (vtype? media? 'err-o-media))
				 text media)))))
(define o (record-constructor option-rcd))
(define make-o (record-constructor
		(make-record-constructor-descriptor option-rtd #f #f)))
(define o? (record-predicate option-rtd))
(define o-text (record-accessor option-rtd 0))
(define o-media (record-accessor option-rtd 1))

;;;;;; Predicates
(define predicate-rtd
  (make-record-type-descriptor 'predicate #f #f #f #f
			       '#((immutable function))))
(define predicate-rcd
  (make-record-constructor-descriptor
   predicate-rtd #f
   (lambda (new)
     (lambda (function)
       (validator new
		  (list (vtype? procedure? 'err-p-function))
		  function)))))
(define p (record-constructor predicate-rcd))
(define p? (record-predicate predicate-rtd))
(define p-function (record-accessor predicate-rtd 0))

;;;;;; Media
(define media-rtd
  (make-record-type-descriptor 'medii #f #f #f #f
			       '#((immutable text)
				  (immutable urls)
				  (immutable books)
				  (immutable images)
				  (immutable videos)
				  (immutable audio))))
(define medii-rcd
  (make-record-constructor-descriptor
   media-rtd #f (lambda (new)
		  (lambda*
		   (#:key (text "") (urls '()) (books '()) (images '())
			  (videos '()) (audio '()))
		   (validator new
			      (list (vstring? 'err-media-text)
				    (vlist? vstring? 'err-media-urls)
				    (vlist? vstring? 'err-media-books)
				    (vlist? vstring? 'err-media-images)
				    (vlist? vstring? 'err-media-videos)
				    (vlist? vstring? 'err-media-audio))
			      text urls books images videos audio)))))
(define media (record-constructor medii-rcd))
(define make-media (record-constructor
		    (make-record-constructor-descriptor media-rtd
							#f
							#f)))
(define media? (record-predicate media-rtd))
(define media-text (record-accessor media-rtd 0))
(define media-urls (record-accessor media-rtd 1))
(define media-books (record-accessor media-rtd 2))
(define media-images (record-accessor media-rtd 3))
(define media-videos (record-accessor media-rtd 4))
(define media-audio (record-accessor media-rtd 5))

;;;;;; Problems
(define problem-rtd
  (make-record-type-descriptor 'prob #f #f #f #f
			       '#((immutable question)
				  (immutable solution)
				  (immutable options)
				  (immutable predicate))))
(define prob-rcd
  (make-record-constructor-descriptor
   problem-rtd #f
   ;; I wanted for it to be possible that module creators need not
   ;; specify the list structure and order of the optional arguments. I
   ;; also did not want to use keyword arguments to reduce the amount
   ;; of repetitive typing. Hence the peculiar protocol below.
   (lambda (new)
     (lambda (q s . args)
       (define (parse-args list)
	 "Return optional arguments (multiple o records and one p
record), from arbitrary positions to the order expected by validator
and the rnrs records definition."
	 (define (pa options predicate remaining)
	   (cond ((null? remaining)
		  (cons (reverse options) predicate))
		 ((p? (car remaining))
		  (pa options
		      (car remaining)
		      (cdr remaining)))
		 (else
		  (pa (cons (car remaining) options)
		      predicate
		      (cdr remaining)))))
	 (pa '() (p equal?) list))
       (let ((op-pair (parse-args args)))
	 (validator new
		    (list (vtype? q? 'err-problem-q)
 			  ((const #t) 'err-problem-s)
			  (vlist? voptions? 'err-problem-os)
			  (vtype? p? 'err-problem-p))
                    ;; Temporary fix to allow multiple solutions
                    #:method 'lazy
		    q s (car op-pair) (cdr op-pair)))))))
(define problem (record-constructor prob-rcd))
(define make-problem (record-constructor
		      (make-record-constructor-descriptor problem-rtd
							  #f
							  #f)))
(define problem? (record-predicate problem-rtd))
(define problem-q (record-accessor problem-rtd 0))
(define problem-s (record-accessor problem-rtd 1))
(define problem-o (record-accessor problem-rtd 2))
(define problem-p (record-accessor problem-rtd 3))

;;;;;; Sets
(define set-rtd
  (make-record-type-descriptor 'set #f #f #f #f
			       '#((immutable id)
				  (immutable contents)
				  (immutable name)
				  (immutable version)
				  (immutable synopsis)
                                  (immutable description)
				  (immutable keywords)
				  (immutable creator)
				  (immutable attribution)
				  (immutable resources)
                                  (immutable logo)
				  (immutable properties))))
;; mecha-set is to be used for non-human set construction (e.g. when
;; pushing through (exchange).
(define mecha-set-rcd
  (make-record-constructor-descriptor set-rtd #f #f))
(define mecha-set (record-constructor mecha-set-rcd))
;; make-set has an enhanced protocol to help humans when creating sets
;; and modules.
(define set-rcd
  (make-record-constructor-descriptor
   set-rtd #f
   ;; Really, when used by humans, sets should be defined through the
   ;; porcelain gset and module commands. make-set is exported for use
   ;; by programmatic module/set construction. The 'user-friendly'
   ;; layer to set creation is hence located in the porcelain
   ;; commands.
   (lambda (new)
     (lambda (id contents name version synopsis description keywords
		 creator attribution resources logo properties)
       (validator new
		  (list (vid? 'err-set-id)
			(vlist? vsets-or-vproblems? 'err-set-contents)
			(vstring? 'err-set-name)
			(vstring? 'err-set-version)
			(vstring? 'err-set-synopsis)
                        (vstring? 'err-set-description)
			(vlist? vstring? 'err-set-keywords)
			(vstring? 'err-set-creator)
			(vlist? vmedia? 'err-set-attribution)
			(vlist? vmedia? 'err-set-resources)
                        (vstring? 'err-set-logo)
			(vlist? vpair? 'err-set-properties))
		  id contents name version synopsis description
                  keywords creator attribution resources logo properties)))))
(define make-set (record-constructor set-rcd))
(define set? (record-predicate set-rtd))
(define set-id (record-accessor set-rtd 0))
(define set-contents (record-accessor set-rtd 1))
(define set-name (record-accessor set-rtd 2))
(define set-version (record-accessor set-rtd 3))
(define set-synopsis (record-accessor set-rtd 4))
(define set-description (record-accessor set-rtd 5))
(define set-keywords (record-accessor set-rtd 6))
(define set-creator (record-accessor set-rtd 7))
(define set-attribution (record-accessor set-rtd 8))
(define set-resources (record-accessor set-rtd 9))
(define set-logo (record-accessor set-rtd 10))
(define set-properties (record-accessor set-rtd 11))

(define (rootset? set)
  "Return #t if set-contents contains problems (which means it's a
rootset). #f otherwise."
  (if (set? set)
      (match (set-contents set)
        ((? null?) #t)
        (((? problem?) ...) #t)
        (_ #f))
      #f))
(define (module? set)
  "Return #t if set is a module, #f otherwise."
  (if (set? set)
      (let ((props (assoc 'module (set-properties set))))
        (and props (cdr props)))
      #f))
(define (tutorial? set)
  "Return #t if set is a tutorial, #f otherwise."
  (if (set? set)
      (let ((props (assoc 'tutorial (set-properties set))))
        (and props (cdr props)))
      #f))

;;;;; Validators
;;;; VALIDATOR and VALIDATE work together to try to detect mistakes in
;;;; module definitions easily made by humans. I have tried to make
;;;; the DSL for module definition as intuitive as possible, but
;;;; inevitably things will get confusing, especially for
;;;; newbies. VALIDATOR should catch most errors in module definition
;;;; when the modules are first loaded (ideally when installing
;;;; through the glean 'installer' functionality). Errors that I
;;;; envisage being captured are things like not encapsulating a
;;;; module's problems in a list, or not using a string type for the
;;;; version of a module.
;;;;
;;;; To locate errors I have decided to make use of VEDICATES,
;;;; essentially predicates whose role it is to return a custom
;;;; symbol, instead of false, when conditions are not met (see below
;;;; for details).
(define* (validator constructor vedicates
		    #:key (method 'strict) . args)
  "Return the completed record, or bail out by deferring to
error-parser, if METHOD is 'strict, and a record definition error is
detected."
  (cond ((eqv? method 'strict)
	 (let ((result (validate vedicates args)))
	   (if (eq? result #t)
	       (apply constructor args)
	       (error-parser result))))
	((eqv? method 'lazy)
         ;; First remove #:method 'lazy from args, then use args.
	 (apply constructor (cddr args)))
	(else
	 (error
	  (string-append "validator: METHOD not recognised: "
			 (object->string method))))))

(define (validate vedicates args)
  "Return #t or the first symbol returned by a VEDICATE.

Check the list of VEDICATES is the same length as the list of ARGS. If
so, test each ARG in turn with the corresponding VEDICATE.

A VEDICATE is a predicate that returns either #t or a symbol that can
be used for key value lookups."
  (define (test-arg vedicate arg)
    (vedicate arg))

  (let ((nr-vedicates (length vedicates))
	(nr-args (length args)))
    (cond ((= nr-vedicates nr-args)
	   (let ((result (map test-arg vedicates args)))
	     (if (null? (flatten (strip result)))
		 #t
		 (flatten (strip result)))))
	  ((< nr-vedicates nr-args)
	   'too-many-args)
	  ((> nr-vedicates nr-args)
	   'too-few-args)
	  (else (error "validate: logic problem.")))))

(define (strip list)
  "Return '() or a list containing only those values that do not
resolve to exactly #t. Can operate on lists of arbitrary depth.

The resulting list could be harvested for, for instance, key value
lookups, if the resulting list contains only symbols."

  (define (stripper next rest)
    (if (list? next)
	(let ((result (strip next)))
	  (if (null? result)
	      (strip rest)
	      (cons result
		    (strip rest))))
	(if (not (eq? next #t))
	    (cons next
		  (strip rest))
	    (strip rest))))

  (if (null? list)
      '()
      (stripper (car list) (cdr list))))

;;;;; Vedicates
;;;; The idea is that one could accumulate a list of
;;;; all these symbols first, and then return a 'report' generated
;;;; through error-parser, detailing all areas of the module that need
;;;; correction, rather than bailing out at the first #f. This is not
;;;; quite working yet — we still bail out at the first error.
;;;;
;;;; Nonetheless, Vedicates and Validator allow for pretty precise
;;;; pin-pointing of content error and the output of human advice
;;;; through error parser.
(define (vlist? child-vedicate symbol)
  "Vlist is a special vedicate, it checks whether the content to be
investigated is a list and then performs a validate on the content of
the list using CHILD-VEDICATE and SYMBOL.

If it notices content is not a list, it returns SYMBOL prefixed with
vlist-."
  (lambda (obj)
    (cond ((null? obj)
	   #t)
	  ((list? obj)
	   (let ((result
		  (map (lambda (ob)
			 (validate (list (child-vedicate symbol))
				   (list ob)))
		       obj)))
	     (if (null? (flatten (strip result)))
		 #t
		 (flatten (strip result)))))
	  (else (string->symbol 
		 (string-append "vlist-"
				(symbol->string symbol)))))))

;;;; Vedicates testing for specific content, defined in terms of the
;;;; more general vedicates below.
(define (vid? symbol)
  (vtype? symbol? symbol))
(define (vpair? symbol)
  (vtype? pair? symbol))
(define (vstring? symbol)
  (vtype? string? symbol))
(define (vboolean? symbol)
  (vtype? boolean? symbol))
(define (vmedia? symbol)
  (vtype? media? symbol))
(define (vsets? symbol)
  (vtype? set? symbol))
(define (voptions? symbol)
  (vtype? o? symbol))
(define (vproblems? symbol)
  (vtype? problem? symbol))
(define (vsets-or-vproblems? symbol)
  (vtype-2? set? problem? symbol))

;;;; General Vedicates for composition.
(define (vtype? predicate symbol)
  "Return a function which takes an object and returns #t if predicate
returns true, or symbol otherwise."
  (lambda (obj)
    (if (predicate obj)
	#t
	symbol)))

(define (vtype-2? predicate1 predicate2 symbol)
  "Return a function which takes an object and returns #t if
predicate1 or predicate2 returns true. Return symbol otherwise.

Has potential to be abstracted to a more powerful compositing
vedicate, but not required at present."
  (lambda (obj)
    (if (or (predicate1 obj)
	    (predicate2 obj))
	#t
	symbol)))

;;;;; Error Parsing
(define (error-parser results)
  "Collection of error messages related to the creation of sets and
modules."
  (define (translate current)
    (cond ((eqv? current 'err-q-text)
	   "Questions must contain question text, which should have
resolved to a string by this point.

Please check your q text field(s).")
	  ((eqv? current 'err-q-media)
	   "Questions can contain rich media. These should take the
form of a single list following the '#:media' keyword containing media
records.

Please check your q media field(s).")
	  ((eqv? current 'err-o-text)
	   "Options, if present, must contain question text, which
should have resolved to a string by this point.

Please check your o text field(s).")
	  ((eqv? current 'err-o-media)
	   "Options can contain rich media. These should take the form
of a single list following the '#:media' keyword containing media
records.

Please check your o media field(s).")
	  ((eqv? current 'err-s-text)
	   "Solutions must contain question text, which should have
resolved to a string by this point.

Please check your s text field(s).")
	  ((eqv? current 'err-s-media)
	   "Solutions can contain rich media. These should take the
form of a single list following the '#:media' keyword containing media
records.

Please check your s media field(s).")
	  ((eqv? current 'err-p-function)
	   "Predicates, if present, should resolve to a lambda, or
procedure.

Please check your p function field(s).")
	  ((eqv? current 'err-s-text)
	   "Media records can contain an introductory paragraph. This
must take the form a single text string following the media record's
'#:text' keyword.

Please check your media text field(s).")
	  ((eqv? current 'err-media-urls)
	   "Media records can contain different references to rich
media. URLs should take the form of text strings contained in a single
list following the '#:books' keyword.

Please check your media urls field(s).")
	  ((eqv? current 'err-media-books)
	   "Media records can contain different references to rich
media. Book references should take the form of text strings contained
in a single list following the '#:books' keyword.

Please check your media books field(s).")
	  ((eqv? current 'err-media-images)
	   "Media records can contain different references to rich
media. Image references should take the form of text strings contained
in a single list following the '#:images' keyword.

Please check your media images field(s).")
	  ((eqv? current 'err-media-videos)
	   "Media records can contain different references to rich
media. Video references should take the form of text strings contained
in a single list following the '#:videos' keyword.

Please check your media videos field(s).")
	  ((eqv? current 'err-media-audio)
	   "Media records can contain different references to rich
media. Audio references should take the form of text strings contained
in a single list following the '#:audio' keyword.

Please check your media audio field(s).")
	  ((eqv? current 'err-problem-q)
	   "Problematic problem-q field value: problem records must
contain a q record and an s record, and can optionally contain as many
o records as desired and up to one p record.

Please check your problem q field(s).")
	  ((eqv? current 'err-problem-s)
	   "Problematic problem-s field value: problem records must
contain a q record and an s record, and can optionally contain as many
o records as desired and up to one p record.

Please check your problem s field(s).")
	  ((eqv? current 'err-problem-os)
	   "Problematic problem-o field value: problem records must
contain a q record and an s record, and can optionally contain as many
o records as desired and up to one p record.

Please check your problem o field(s).")
	  ((eqv? current 'err-problem-p)
	   "Problematic problem-p field value: problem records must
contain a q record and an s record, and can optionally contain as many
o records as desired and up to one p record.

Please check your problem p field(s).")
	  ((eqv? current 'err-set-id)
	   "Sets and modules must contain an id. This field's value
should be a symbol.

Please check your sets' and modules' id field(s).")
	  ((eqv? current 'err-set-contents)
	   "Sets and modules normally contain problem or set
contents. They are stored in a list in the '#:contents' field. Each
list member should be a problem or a set.

Please check your sets' and modules' contents field(s).")
	  ((eqv? current 'err-set-name)
	   "Set and Module records can contain a name field. This must
take the form a single text string following the set or module
record's '#:name' keyword.

Please check your sets' and modules' name field(s).")
	  ((eqv? current 'err-set-version)
	   "Set and Module records can contain a version field. This must
take the form a single text string following the set or module
record's '#:version' keyword.

Please check your sets' and modules' version field(s).")
	  ((eqv? current 'err-set-synopsis)
	   "Set and Module records can contain a synopsis field. This
must take the form a single text string following the set or module
record's '#:synopsis' keyword.

Please check your sets' and modules' synopsis field(s).")
	  ((eqv? current 'err-set-description)
	   "Set and Module records can contain a description
field. This must take the form a single text string following the set
or module record's '#:description' keyword.

Please check your sets' and modules' description field(s).")
	  ((eqv? current 'err-set-creator)
	   "Set and Module records can contain a creator field. This
must take the form a single text string following the set or module
record's '#:creator' keyword.

Please check your sets' and modules' creator field(s).")
	  ((eqv? current 'err-set-attribution)
	   "Set and Module records can contain attribution
references. These should take the form of a single list following the
set or module record's '#:attribution' keyword. The list should
contain only media records.

Please check your sets' and modules' attribution field(s).")
          ((eqv? current 'err-set-resources)
	   "Set and Module records can contain attribution
references. These should take the form of a single list following the
set or module record's '#:resources' keyword. The list should contain
only media records.

Please check your sets' and modules' resources field(s).")
	  ((eqv? current 'err-set-module)
	   "The contents of the '#:module' field should be a boolean #t or #f,
to indicate whether it should be advertised as a module or not.")
	  ((eqv? current 'err-set-logo)
	   "The contents of the '#:logo' field should be a plain string URI
pointing to where the logo image can be retrieved.")
	  ((eqv? current 'vlist-err-set-contents)
	   "The set and/or problem contents of a Set or Module record
should be encapsulated in a list, after the '#:contents' keyword. This
does not seem to be the case with at least one of sets in this record
set.

Please check your sets' and modules' contents field(s).")
	  ((eqv? current 'vlist-err-set-attribution)
	   "The attribution contents of a Set or Module record should
be encapsulated in a list, after the '#:attribution' keyword. This
does not seem to be the case with at least one of sets in this record
set.

Please check your sets' and modules' attribution field(s).")
          ((eqv? current 'vlist-err-set-resources)
	   "The resources contents of a Set or Module record should be
encapsulated in a list, after the '#:resources' keyword. This does not
seem to be the case with at least one of sets in this record set.

Please check your sets' and modules' resources field(s).")
	  ((eqv? current 'vlist-err-media-urls)
	   "The URL contents of a media record should be encapsulated
in a list, after the '#:urls' keyword. This does not seem to be the
case with at least one of the media records in this record set.

Please check your media records' URLs field(s).")
	  ((eqv? current 'vlist-err-media-books)
	   "The book contents of a media record should be encapsulated
in a list, after the '#:books' keyword. This does not seem to be the
case with at least one of the media records in this record set.

Please check your media records' books field(s).")
	  ((eqv? current 'vlist-err-media-images)
	   "The image contents of a media record should be
encapsulated in a list, after the '#:images' keyword. This does not
seem to be the case with at least one of the media records in this
record set.

Please check your media records' images field(s).")
	  ((eqv? current 'vlist-err-media-videos)
	   "The video contents of a media record should be
encapsulated in a list, after the '#:videos' keyword. This does not
seem to be the case with at least one of the media records in this
record set.

Please check your media records' videos field(s).")
	  ((eqv? current 'vlist-err-media-audio)
	   "The audio contents of a media record should be
encapsulated in a list, after the '#:audio' keyword. This does not
seem to be the case with at least one of the media records in this
record set.

Please check your media records' audio field(s).")
	  (else (string-append "Unknown error: "
			       (object->string current)))))

  (if (list? results)
      (throw 'validation (map translate results))
      (error "error-parser: logic error.")))
