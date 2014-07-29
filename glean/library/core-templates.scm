;; core-templates.scm --- Content creation templates.    -*- coding: utf-8 -*-
;;
;; Copyright © 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 15 Jul 2014
;;
;; This file is part of Glean.
;;
;; Glean is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; Glean is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with glean; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; Provide a convenience layer for creating content.  This layer should be
;; used when actually writing content.  It will in turn use the records
;; defined in library/sets.scm.
;;
;; This module provides more convenient 'templates' for creating content, and
;; aims to provide a solid parser for newly created content to allow users to
;; obtain meaningful analysis of where their modules might be going wrong.
;;
;; Templates exposed here provide dynamic inheritance from parent sets;
;; i.e. fields such as 'creator' are automatically populated for all child
;; sets with its parent's value if the child set does not have a 'creator'
;; value.
;;
;; The template parsing and inheritance models are constructed using the
;; set-monad, which is essentially a 'maybe-monad', allowing us to analyse
;; without the use of exceptions.
;;
;;; Code:

(define-module (glean library core-templates)
  #:use-module (glean common monads)
  #:use-module (glean library sets)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export     (
                resolve-set
                tutorial
                chapter
                set
                module
                problem
                q
                s
                o
                p
                media
                ))

(define* (example)
  "Return an example tutorial set."
  (resolve-set
   (tutorial 'test
             #:name "name"
             #:version "0.1"
             #:synopsis "synopsis"
             #:description "description"
             #:keywords '("key1" "key2")
             #:creator "creator"
             #:attribution '()
             #:resources '()
             #:logo "logo"
             #:properties '()
             #:chapters `(,(chapter "Title1" '("part1.1" "part1.2")
                                    "synopsis")
                          ,(chapter "Title2" '("part2.1")))
             #:completion (chapter "Outro" '("Congratulations!")))))


;;;; Set-Monad Definition
;;;
;;; The set-monad is, for now, a simple 'maybe-monad': it checks its mvalue to
;;; see whether it is a nothing. If so it returns the nothing immediately
;;; instead of passing it to mproc for application.
;;;
;;; Shortcomings:
;;;
;;; - Currently no logging is built in.
;;; - The way the nothing interacts with the monad and the defined monadic
;;;   procedures results in parsing ending at the first error found.  Ideally
;;;   the parser should traverse the entire 'module definition' and report on
;;;   all errors found, providing contextual information for each error.

(define (set-return value)
  "Return an mvalue for the set-monad, which is simply a procedure wrapped
around value."
  (lambda () value))

(define (set-bind mvalue mproc)
  "Bind for the set-monad simply unwraps its MVALUE and checks whether it is a
'nothing'.  If so it returns the nothing immediately.  Otherwise it passes the
unwrapped MVALUE to its MPROC."
  (lambda ()
    (let ((candidate (mvalue)))
      (if (nothing? candidate)
          candidate
          ((mproc candidate))))))

(define-monad set-monad
  (bind   set-bind)
  (return set-return))

;;;; Inheritance Workhorse
;;;
;;; Inheritance is implemented by means of currying set manufacture: the
;;; additional lambda allows us to take into account potential values passed in
;;; at the application stage by parent modules.

(define-record-type <transient-set>
  (make-transient-set maker)
  transient-set?
  (maker transient-set-maker))

(define (transient-set meta properties contents)
  "Delay actual set creation by returning a 'case-lambda', simply forcing set
creation if applied without arguments, or 'merging' the set's field values
with those provided as arguments if they are present."
  (define (dance meta prop cont)
    "Return the set as it will finally appear in the library.  Currently
\"dance\"'s primary role is to re-arrange arguments in such a way as to pass
contents as the second arg to make-set."
    (match (list meta prop cont)
      (((id name vers syno desc keyw crea attr reso logo) prop cont)
       (make-set id cont name vers syno desc keyw crea attr reso logo prop))
      (_ (error "DANCE -- unexpected combination of meta, prop, cont!"
                meta prop cont))))
  (lambda ()
    (make-transient-set
     (case-lambda
       (()
        (dance meta properties (reify-contents meta properties contents)))
       ((base-meta base-properties)
        (let ((x-meta (extend-meta meta base-meta))
              (x-prop (extend-properties properties base-properties)))
          (dance x-meta x-prop (reify-contents x-meta x-prop contents))))))))


;;;; High Level Porcelain Commands
;;;
;;; These procedures are the ones that are actually to be used by content
;;; creators.  CHAPTER should normally only be used with tutorial, but is
;;; currently implemented as a procedure, so it could be used anywhere.  In
;;; future, TUTORIAL and CHAPTER may be implemented using macros.

(define* (chapter title parts #:optional (synopsis ""))
  "Return a set for use in tutorials.  Id for the set will be derived from the
string TITLE.  Contents will be derived by transforming the list of strings
PARTS into one 'informational problem' each.  Each string thus represents a
segment in the module.  Finally, the optional synopsis will provide a synopsis
for this 'tutorial chapter'."
  (define (parts->problems parts)
    (map (lambda (string) (problem (q string) #f)) parts))
  (define (monadic-check-name obj)
    (lambda () (validate-name obj)))
  (define (monadic-check-parts obj)
    (lambda () (validate-parts obj)))

  ((mlet* set-monad
       ((name (monadic-check-name title))
        (cpar (monadic-check-parts parts))
        (meta (monadic-check-meta (string->symbol name) title "" synopsis ""
                                  '() "" '() '() ""))
        (prop (monadic-check-properties '() 'tutorial))
        (cont (monadic-check-contents (parts->problems cpar)))
        (set  (transient-set meta prop cont)))
     (return set))))

(define* (tutorial id #:key (name "") (version "") (synopsis "")
                   (description "") (keywords '()) (creator "")
                   (attribution '()) (resources '()) (logo "")
                   (properties '())
                   (chapters '())       ; list of (chapters)
                   (completion          ; optional tutorial completion chapter
                    (chapter "Completion"
                             '("You have completed this tutorial and can now
move on to further topics."))))
  "Return a set tagged as a tutorial, with the list of results from the
chapter procedure in CHAPTERS as the individual sections of the tutorial, and
the result of the chapter procedure in COMPLETION as the outro.  If COMPLETION
is not provided a generic completion message will be provided."
  ((mlet* set-monad
       ((meta       (monadic-check-meta id name version synopsis description
                                        keywords creator attribution resources
                                        logo))
        (properties (monadic-check-properties properties 'tutorial))
        (contents   (monadic-check-contents (append chapters (list completion))))
        (set        (transient-set meta properties contents)))
     (return set))))

(define* (set id #:key (name "") (version "") (synopsis "") (description "")
              (keywords '()) (creator "") (attribution '()) (resources '())
              (logo "") (properties '()) (contents '()))
  "Return a simple set capable of inheriting values that are not provided,
according to rules, from parent sets."
  ((mlet* set-monad
       ((meta       (monadic-check-meta id name version synopsis description
                                        keywords creator attribution resources
                                        logo))
        (properties (monadic-check-properties properties))
        (contents   (monadic-check-contents contents))
        (set        (transient-set meta properties contents)))
     (return set))))

(define* (module id #:key (name "") (version "") (synopsis "")
                 (description "") (keywords '()) (creator "")
                 (attribution '()) (resources '()) (logo "") (properties '())
                 (contents '()))
  "Return a set tagged as a module, constructed out of the supplied
arguments.  As modules normally form the top level of a discipline, as much
information should be placed in the respective fields as possible. This
information can then be inherited by child sets and modules."
  ((mlet* set-monad
       ((meta       (monadic-check-meta id name version synopsis description
                                        keywords creator attribution resources
                                        logo))
        (properties (monadic-check-properties properties 'module))
        (contents   (monadic-check-contents contents))
        (set        (transient-set meta properties contents)))
     (return set))))

;;;;; Re-exports?
;;;
;;; Procedures currently re-exported from (glean library sets).
;;; These procedures are likely to be developed into more convenient content
;;; creation templates.

;; Problem creation
(define (problem . args) (apply (@ (glean library sets) problem) args))

;; Question creation
(define (q . args) (apply (@ (glean library sets) q) args))

;; Solution creation
(define (s . args) (apply (@ (glean library sets) s) args))

;; Option creation
(define (o . args) (apply (@ (glean library sets) o) args))

;; Predicate creation
(define (p . args) (apply (@ (glean library sets) p) args))

;; Media creation
(define (media . args) (apply (@ (glean library sets) media) args))

;;;; Initiator
;;;
;;; Used by glean to start the reification of the given transient-set (hopefully) or
;;; nothing, into a final set or just a nothing.

(define (resolve-set maybe)
  "Return either a fully resolved set, with all its children, or a nothing
value, if MAYBE failed to compile properly.

This procedure will be used by the library-store and potential high level
discipline analysers, such as the import-module functionality."
  (if (transient-set? maybe) ((transient-set-maker maybe)) maybe))


;;;; Convenience For Manipulating Properties

(define (properties-tutorial-add properties)
  "Return a set of properties with the tutorial property added if it does not
yet contain it."
  (properties-add 'tutorial #t properties))

(define (properties-module-add properties)
  "Return a set of properties with the tutorial property added if it does not
yet contain it."
  (properties-add 'module #t properties))

(define (properties-add property value properties)
  "Return a set of properties, ensuring PROPERTY exists within PROPERTIES,
adding it with VALUE if not."
  (if (assoc property properties)
      properties
      (acons property value properties)))

;;;;; High Level Monadic Validators

(define (monadic-check-meta . args)
  "Return a procedure which, when applied, will return ARGS as a list or a
nothing if ARGS did not pass validation."
  (lambda ()
    (let ((validators (list validate-id validate-name validate-version
                            validate-synopsis validate-description
                            validate-keywords validate-creator
                            validate-attribution validate-resources
                            validate-logo)))
      (if (= (length validators) (length args))
          (let ((result (map (lambda (validator value)
                               (validator value))
                             validators args)))
            (if (null? (filter nothing? result))
                result
                (nothing 'meta result)))
          (error "MONADIC-CHECK-META -- args is of unexpected length."
                 args)))))

(define* (monadic-check-properties properties #:optional operation)
  "Return a procedure which, when applied, will return PROPERTIES, optionally
enhanced by OPERATION, or a nothing if PROPERTIES did not pass validation.

OPERATION should be a symbol indicating a field to add to
properties. Currently 'tutorial and 'module are supported."
  (lambda ()
    (let ((props (validate-properties properties)))
      (cond ((or (nothing? props) (not operation)) props)
            ((eq? operation 'tutorial) (properties-tutorial-add props))
            ((eq? operation 'module)   (properties-module-add props))
            (else (error "MONADIC-CHECK-PROPERTIES -- unknown operation"
                         operation))))))

(define (monadic-check-contents . contents)
  "Return a procedure which, when applied, will return CONTENTS or a nothing
if CONTENTS did not pass validation."
  (define (validate-contents obj)
    ((list-validate validate-content 'content-list) obj))
  (define (validate-content obj)
    (type-validate (lambda (obj)
                     (or (transient-set? obj) (problem? obj)))
                   'content obj))
  (lambda ()
    (validate-contents
     (fold (lambda (current result)
             (if (list? current)
                 (append result current)
                 (append result (list current))))
           '() contents))))

;;;;; High Level Resolvers

(define (reify-contents meta prop contents)
  "Return a list of reified contents.  For each item in CONTENTS, check if it
is a problem or a transient-set.  If the former simply return it.  If the
latter, extract the transient-set's procedure and apply it to META and PROP,
in order to add the resulting set to the list of contents to be returned."
  (map (lambda (content)
         (cond ((transient-set? content)
                ((transient-set-maker content) meta prop))
               ((problem? content) content)
               (else (error "REIFY-CONTENTS -- unexpected content" content))))
       contents))

(define (extend-meta naive-meta base-meta)
  "Return a merged group of meta-information built by combining NAIVE-META and
BASE-META.  NAIVE-META is the group of meta-information as provided in the
porcelain commands in this module.  BASE-META is the group inherited from the
parent of the set containing NAIVE-META."
  (let ((resolvers (list resolve-id resolve-name resolve-version
                         resolve-synopsis resolve-description resolve-keywords
                         resolve-creator resolve-attributions
                         resolve-resources resolve-logo)))
    (if (apply = (map length (list resolvers naive-meta base-meta)))
        (map (lambda (resolver base-value naive-value)
               (resolver base-value naive-value))
             resolvers base-meta naive-meta)
        (error "EXTEND-META -- unexpected length for naive-meta or base-meta"
               naive-meta base-meta))))

(define (extend-properties naive-properties base-properties)
  "Return a merged group of properties built by combining NAIVE-PROPERTIES and
BASE-PROPERTIES.  NAIVE-PROPERTIES is the group of properties as provided by
the creator to the porcelain commands in this module.  BASE-PROPERTIES are
inherited from the parent of the set containing NAIVE-PROPERTIES."
  (resolve-properties base-properties naive-properties))


;;;;; Rules For Base + Naive Resolution

(define (resolve-id base-id naive-id)
  "Return a merged id field.

IDs are mandatory, and as such they should always be populated. What's more,
the naive-id, the id supplied by the creator, should always be used."
  naive-id)

(define (resolve-name base-name naive-name)
  "Return a merged name field.

Names should normally have been provided.  In the case that it has not been,
we can default to base-name + \"Child\".  If it has been provided, we should
defer to naive-name."
  (if (string-null? naive-name)
      (string-append base-name " Child")
      naive-name))

(define (resolve-version base-version naive-version)
  "Return a merged version field.

Versions are optional.  If they have not been provided in NAIVE, we should
inherit from BASE.  If both have been provided, we should use NAIVE."
  (if (string-null? naive-version) base-version naive-version))

(define (resolve-synopsis base-synopsis naive-synopsis)
  "Return a merged synopsis field.

Synopsi are optional.  If they have not been provided in NAIVE then we should
leave this field empty."
  naive-synopsis)

(define (resolve-description base-description naive-description)
  "Return a merged description field.

Descriptions are optional.  If they have not been provided in NAIVE then we
should leave this field empty."
  naive-description)

(define (resolve-keywords base-keywords naive-keywords)
  "Return a merged keywords field.

Keywords are optional.  However, NAIVE should always extend BASE."
  (append base-keywords naive-keywords))

(define (resolve-creator base-creator naive-creator)
  "Return a merged creator field.

Creators are optional.  If they have not been provided in NAIVE then we
should inherit from base."
  (if (string-null? naive-creator) base-creator naive-creator))

(define (resolve-attributions base-attributions naive-attributions)
  "Return a merged attributions field.

Attributionss are optional.  If they have not been provided in NAIVE then we
should inherit from base."
  (if (null? naive-attributions) base-attributions naive-attributions))

(define (resolve-resources base-resources naive-resources)
  "Return a merged resources field.

Resourcess are optional.  If they have not been provided in NAIVE then we
should inherit from base."
  (if (null? naive-resources) base-resources naive-resources))

(define (resolve-logo base-logo naive-logo)
  "Return a merged logo field.

Logos are optional.  If they have not been provided in NAIVE then we
should inherit from base."
  (if (string-null? naive-logo) base-logo naive-logo))

(define (resolve-properties base-properties naive-properties)
  "Return a merged properties field.

Properties are optional.  If they have not been provided in NAIVE then we
should return the-empty-properties list."
  naive-properties)

(define (resolve-contents id cont name vers syno desc keyw crea attr reso
                          logo prop)
  "Return the set arguments, with each entry in CONT expanded to sets or
problems by virtue of each entry, which should be a future-set procedure,
having been applied to all other arguments."
  (list id
        (map (lambda (fut-set)
               (if (problem? fut-set)
                   fut-set
                   (fut-set id name vers syno desc keyw crea attr reso logo
                            prop)))
             cont)
        name vers syno desc keyw crea attr reso logo prop))

;;;;; Validators: Ensure Each Field Contains An Acceptable Value

(define (validator-nothing-label nothing)
  "Return the label for the field that caused the NOTHING to be generated."
  (car (nothing-context nothing)))

(define (validator-nothing-value nothing)
  "Return the value that caused the NOTHING to be generated."
  (cdr (nothing-context nothing)))

(define (type-nothing label value)
  "Return a nothing of type 'type, identifying field LABEL with VALUE as the
culprit."
  (nothing 'type (cons label value)))

(define (list-nothing label value)
  "Return a nothing of type 'list, identifying field LABEL with VALUE as the
culprit.

VALUE will contain at least on further nothing."
  (nothing 'list (cons label value)))

(define (type-validate predicate label value)
  "Check VALUE with PREDICATE, returning a type-nothing with the field we're
checking indicated by LABEL."
  (if (predicate value) value (type-nothing label value)))

(define (list-validate validator label)
  "Return a validator which will check an object to see if it is a list, and
if so, check every member of the list to see if it passes VALIDATOR.

If the object is not a list, raise a 'type nothing, with LABEL as the field
indicator.  If the object is a list but contains a nothing by virtue of the
scan with VALIDATOR, raise a 'list nothing."
  (lambda (object)
    (if (list? object)
        (let ((lst (map validator object)))
          (if (null? (filter nothing? lst))
              lst
              (list-nothing label lst)))
        (type-nothing label object))))

(define (string-validator label)
  "Return a validator which tests an object for stringhood, returning a 'type
nothing with field LABEL if the object fails.  The validator will return the
object otherwise."
  (lambda (object)
    (type-validate string? label object)))

(define (symbol-validator label)
  "Return a validator which tests an object for symbolhood, returning a 'type
nothing with field LABEL if the object fails.  The validator will return the
object otherwise."
  (lambda (object)
    (type-validate symbol? label object)))

(define (pair-validator label)
  "Return a validator which tests an object for pairhood, returning a 'type
nothing with field LABEL if the object fails.  The validator will return the
object otherwise.

This currently is not capable of testing the pair for type."
  (lambda (object)
    (type-validate pair? label object)))

(define (validate-id obj)
  "Return OBJ if it is a valid set-id, a 'type nothing otherwise."
  ((symbol-validator 'id) obj))

(define (validate-name obj)
  "Return OBJ if it is a valid set-name, a 'type nothing otherwise."
  ((string-validator 'name) obj))

(define (validate-version obj)
  "Return OBJ if it is a valid set-version, a 'type nothing otherwise."
  ((string-validator 'version) obj))

(define (validate-synopsis obj)
  "Return OBJ if it is a valid set-synopsis, a 'type nothing otherwise."
  ((string-validator 'synopsis) obj))

(define (validate-description obj)
  "Return OBJ if it is a valid set-description, a 'type nothing otherwise."
  ((string-validator 'description) obj))

(define (validate-keywords obj)
  "Return OBJ if it is a valid set-keywords, a 'type nothing otherwise."
  ((list-validate (string-validator 'keyword) 'keywords) obj))

(define (validate-creator obj)
  "Return OBJ if it is a valid set-creator, a 'type nothing otherwise."
  ((string-validator 'creator) obj))

(define (validate-attribution obj)
  "Return OBJ if it is a valid set-attribution, a 'type nothing otherwise."
  ((list-validate validate-media 'attribution) obj))

(define (validate-resources obj)
  "Return OBJ if it is a valid set-resources, a 'type nothing otherwise."
  ((list-validate validate-media 'resources) obj))

(define (validate-media obj)
  "Return OBJ if it is a valid media, a 'type nothing otherwise."
  (type-validate media? 'media obj))

(define (validate-logo obj)
  "Return OBJ if it is a valid set-logo, a 'type nothing otherwise."
  (type-validate string? 'logo obj))

(define (validate-properties obj)
  "Return OBJ if it is a valid set-properties, a 'type nothing otherwise."
  ((list-validate (pair-validator 'pair) 'properties) obj))

(define (validate-contents obj)
  "Return OBJ if it is a valid set-contents, a 'type nothing otherwise."
  ((list-validate validate-content 'obj) obj))

(define (validate-content obj)
  "Return OBJ if it is a valid future-set procedure, a 'type nothing
otherwise.

Currently this only checks whether OBJ is a procedure. It should also check
what kind of procedure it is."
  (type-validate (lambda (obj)
                   (or (procedure? obj) (problem? obj)))
                 'content obj))

(define (validate-parts obj)
  "Return OBJ if it is a valid part, a 'type nothing otherwise."
  ((list-validate (string-validator 'part) 'parts) obj))

(define (validate-set-parts . args)
  (let ((procs (list validate-id validate-contents validate-name
                     validate-version validate-synopsis validate-description
                     validate-keywords validate-creator validate-attribution
                     validate-resources validate-logo validate-properties)))
    (if (= (length args) (length procs))
        (map (lambda (validator candidate)
               (validator candidate))
             procs args)
        (error "VALIDATE-SET-PARTS -- mismatching number of args."))))

;;; core-templates.scm ends here
