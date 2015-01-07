;; sets.scm --- defining the set data-type    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
;;
;; This file is part of Glean.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Sets are recursive records used to group problems and other sets
;; together for use in Glean.
;;
;; Sets are a means of grouping problems together under a
;; tag. Glean uses the tag to measure progress against a
;; discipline, and will select a "random" question from the
;; algorithmically determined highest priority set.
;;
;; Alternatively, sets can contain other sets — a way to group sets
;; of problems in meta-groups: modules if you will.
;;
;; This module also defines any sub-records used in set/module
;; creation, and as such is glean's primary data-type library.
;;
;; Finally this library provides a first draft at an embedded means of content
;; definition error detection, and a means to provide meaningful help to
;; content creators on the back of this.
;;
;; It is now obvious that these checks should not be carried out at run-time,
;; when the sets are loaded, but by a dedicated lint tool, and/or at
;; installation time.  The current method of linting is preserved, as a
;; reminder of how not to implement the next revision.
;;
;; Finally, I would greatly prefer migrating to srfi-9 records. Would this be
;; feasible? Only once:
;; - linter has been removed from this module
;; - we have removed set transfers from communication tests (these will never
;;   be transferred as is anyway: only lists of set fields will be emitted by
;;   the library via its responses).
;;
;;; Tests:
;; (tests sets)
;;
;;; Code:


;;;;; Module Definition
(define-module (glean library sets)
  #:use-module (glean common utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (
            set
            set?
            rootset?
            module
            plain-module?
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

;;;; Set Structure
;;;
;;; SRFI-9 record type definitions of sets and their component parts.

;;;;; Questions
(define-record-type <question>
  (make-q text media)
  q?
  (text  q-text)
  (media q-media))

(define* (q text #:optional (media (media)))
  (make-q text media))

;;;;; Solutions
(define-record-type <solution>
  (make-s text media)
  s?
  (text  s-text)
  (media s-media))

(define* (s text #:optional (media (media)))
  (make-s text media))

;;;;; Options
(define-record-type <option>
  (make-o text media)
  o?
  (text  o-text)
  (media o-media))

(define* (o text #:optional (media (media)))
  (make-o text media))

;;;;; Predicates
(define-record-type <predicate>
  (make-p function)
  p?
  (function p-function))

(define (p function)
  (make-p function))

;;;;; Media
(define-record-type <media>
  (make-media text urls books images videos audio)
  media?
  (text    media-text)
  (urls    media-urls)
  (books   media-books)
  (images  media-images)
  (videos  media-videos)
  (audio   media-audio))

(define* (media #:key (text "") (urls '()) (books '()) (images '())
                (videos '()) (audio '()))
  (make-media text urls books images videos audio))

;;;;; Problems
(define-record-type <problem>
  (make-problem question solution options predicate)
  problem?
  (question   problem-q)
  (solution   problem-s)
  (options    problem-o)
  (predicate  problem-p))

;; I wanted for it to be possible that module creators need not
;; specify the list structure and order of the optional arguments. I
;; also did not want to use keyword arguments to reduce the amount
;; of repetitive typing. Hence the peculiar protocol below.
(define (problem q s . args)
  (define (parse-args list)
    "Return optional arguments (multiple o records and one p record), from
arbitrary positions to the order expected by validator & the record
definition."
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
    (make-problem q s (car op-pair) (cdr op-pair))))

;;;;; Sets
(define-record-type <set>
  (mecha-make-set id contents name version synopsis description keywords
                  creator attribution resources logo properties lineage)
  set?
  (id           set-id)
  (contents     set-contents)
  (name         set-name)
  (version      set-version)
  (synopsis     set-synopsis)
  (description  set-description)
  (keywords     set-keywords)
  (creator      set-creator)
  (attribution  set-attribution)
  (resources    set-resources)
  (logo         set-logo)
  (properties   set-properties))

(define* (make-set id #:optional (contents '()) (name "") (version "")
                   (synopsis "") (description "") (keywords '()) (creator "")
                   (attribution '()) (resources '()) (logo "")
                   (properties '()) (lineage #f))
  (mecha-make-set id contents name version synopsis description keywords
                  creator attribution resources logo properties lineage))

(define (rootset? set)
  "Return #t if set-contents contains problems (which means it's a
rootset?). #f otherwise."
  (if (set? set)
      (match (set-contents set)
        ((? null?) #t)
        (((? problem?) ...) #t)
        (_ #f))
      #f))

(define (plain-module? set)
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


;;;; Error Parsing
;;;
;;; These are useful as a preliminary formal specification of sets.

(define (error-parser results)
  "Collection of error messages related to the creation of sets and
modules."
  (define (translate current)
    (cond ((eqv? current 'err-q-text)
           "Questions must contain question text, which should have
resolved to a string by this point.

Please check your 'q' text field(s).")
          ((eqv? current 'err-q-media)
           "Questions can contain rich media. These should take the
format of a single list following the '#:media' keyword containing media
records.

Please check your 'q' media field(s).")
          ((eqv? current 'err-o-text)
           "Options, if present, must contain question text, which
should have resolved to a string by this point.

Please check your 'o' text field(s).")
          ((eqv? current 'err-o-media)
           "Options can contain rich media. These should take the form
of a single list following the '#:media' keyword containing media
records.

Please check your 'o' media field(s).")
          ((eqv? current 'err-s-text)
           "Solutions must contain question text, which should have
resolved to a string by this point.

Please check your 's' text field(s).")
          ((eqv? current 'err-s-media)
           "Solutions can contain rich media. These should take the
format of a single list following the '#:media' keyword containing media
records.

Please check your 's' media field(s).")
          ((eqv? current 'err-p-function)
           "Predicates, if present, should resolve to a lambda, or
procedure.

Please check your 'p' function field(s).")
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
           "Problematic problem-q field value: problem records must contain a
'q' record and an 's' record, and can optionally contain as many 'o' records
as desired and up to one 'p' record.

Please check your problem 'q' field(s).")
          ((eqv? current 'err-problem-s)
           "Problematic problem-s field value: problem records must contain a
'q' record and an 's' record, and can optionally contain as many 'o' records
as desired and up to one 'p' record.

Please check your problem 's' field(s).")
          ((eqv? current 'err-problem-os)
           "Problematic problem-o field value: problem records must contain a
'q' record and an 's' record, and can optionally contain as many 'o' records
as desired and up to one 'p' record.

Please check your problem 'o' field(s).")
          ((eqv? current 'err-problem-p)
           "Problematic problem-p field value: problem records must contain a
'q' record and an 's' record, and can optionally contain as many 'o' records
as desired and up to one 'p' record.

Please check your problem 'p' field(s).")
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


;;;; Set Usage
;;; This section details porcelain commands that should normally be
;;; used by humans to generate sets and/or modules.

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

;;; sets.scm ends here
