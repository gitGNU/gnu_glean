;; lexp.scm --- discipline identifiers    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 26 December 2014
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
;; lexps are expressions akin to xpath expressions.  Their aim is to make it
;; easy to traverse disciplines and to retrieve arbitrary sets within them.
;;
;; lexps are a record type, but a macro, `lexp' is provided which allows easy
;; generation of lexps.  The recommended format for user lexp generation is:
;; (lexp (discipline [child-set [grandchild-set [...]]]))
;;
;; lexps will initially be used to generate upgrade paths for disciplines
;; when, such that the lounge is able to remain in-sync with changes to a
;; discipline's structure, without it requiring knowledge of a full discipline.
;;
;; At a later stage lexps will be used to facilitate aribitrary discipline/set
;; referencing in set properties (e.g. user progression prerequisites in
;; disciplines that take the form of a minimum rank in another discipline).
;;
;;; Code:

(define-module (glean library lexp)
  #:use-module (glean common utils)
  #:use-module (glean library sets)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export     (<lexp>
                lexp
                lexp?
                lexp-base
                lexp-make
                lexp-rest
                lexp-append
                lexp-serialize
                lexp-set-resolve
                lexp-set-member
                set-lexp
                set-child-lexps
                <node>
                make-node
                node?
                node-children
                node-id
                leaf?
                serialize-node
                discipline-tree
                discipline-tree-base
                discipline-tree->serialized
                discipline-serialized->tree
                discipline-serialized-tree))


;;;; Section Heading 1
;;;
;;; Section Commentary 1

(define-immutable-record-type <lexp>
  (lexp-mecha-make base rest)
  lexp?
  (base lexp-base)
  (rest lexp-rest set-lexp-rest))

(define (lexp-make base-or-full . rest-or-null)
  "Return a lexp, an expression which can be used to retrieve a specific
discipline within a library using simple list syntax.  BASE-OR-FULL can be a
symbol, identifying the starting point for the lexp path, or a list containing
the entire lexp path as symbols.  REST-OR-NULL will be a list of symbols if
BASE-OR-FULL is a symbol, or null if it is a list.

Example:

 (lexp-make '(git init))

This lexp can resolve to the init set located in the git discipline."
  (cond ((symbol? base-or-full)         ; symbol(s)->lexp
         (match rest-or-null
           ((or () ((? symbol?) ...))
            (lexp-mecha-make base-or-full rest-or-null))
           (_ (throw 'glean-type-error "LEXP-MAKE: Unexpected REST-OR-NULL"
                     rest-or-null))))
        ((null? rest-or-null)           ; list->lexp
         (match base-or-full
           (((? symbol? base) . ((? symbol? rest) ...))
            (lexp-mecha-make base rest))
           (_ (throw "LEXP-MAKE: Unexpected BASE-OR-FULL" base-or-full))))
        (else (throw 'glean-type-error
                     "LEXP-MAKE: Unexpected ingredient pattern."
                     base-or-full rest-or-null))))

;; syntactic sugar for lexps.
(define-syntax lexp
  (syntax-rules ()
    ((lex exp)
     (lexp-make 'exp))))

(define (lexp-serialize lxp)
  "Return the full lexp contained in LXP, as would have been provided when
generating it."
  (match lxp
    (($ <lexp> base rest)
     (cons base rest))))

(define (lexp-append lxp . ids)
  "Append IDS to LXP's lxp-rest."
  (set-lexp-rest lxp (append (lexp-rest lxp) ids)))

;;;; Set lexp operations

(define (lexp-set-resolve set lxp)
  "Extract the set identified by the lexp LXP from SET, or a nothing value
with id 'lexp-unknown, and context SET and LXP."
  (define (resolve next-set next-lxp)
    (match next-lxp
      ;; LEXP matches this set, and no further recursion needed.
      (($ <lexp> (? (cut eqv? <> (set-id next-set))) ())
       next-set)
      ;; LEXP matches this set and we should recurse if possible.
      (($ <lexp> (? (cut eqv? <> (set-id next-set)))
          (= lexp-make rest))
       (match (lexp-set-member next-set rest)
         ;; Recursion possible, recurse
         ((? set? mtch) (lexp-set-resolve mtch rest))
         ;; Rest of NEXT-LXP does not point to set in SET.
         (_ (nothing 'lexp-unknown `(,(set-id set) ,lxp)))))
      ;; NEXT-LXP's base does not match NEXT-SET id.
      (($ <lexp>) (nothing 'lexp-unknown `(,(set-id set) ,lxp)))))

  (if (and (set? set) (lexp? lxp))
      (resolve set lxp)
      (throw 'glean-type-error "LEXP-SET-RESOLVE: Unexpected SET or LEXP:"
             set lxp)))

(define (lexp-set-member set lxp)
  "Return the set that is a child of SET which matches the base of LXP, or
#f."
  (if (rootset? set)
      #f                                ; rootset or no children
      (match (fold (lambda (disc result)
                     (cond (result result)
                           ((eqv? (lexp-base lxp) (set-id disc)) disc)
                           (else #f)))
                   #f
                   (set-contents set))
        ((? set? mtch) mtch)         ; We have found our matching set
        (_ #f))))                    ; We were unable to find our matching set

(define (set-lexp set)
  "Return base-lexp generated from SET."
  (lexp-make (set-id set)))

(define* (set-child-lexps set #:optional base-lxp)
  "Return the list of lexps for the child-sets of SET, or the empty list if
set is a rootset.  Assume set is a crownset, or, if LXP is provided, use it as
the basis for generating the lexp list."
  (cond ((rootset? set) '())
        ((set? set)
         (map (lambda (child)
                (if base-lxp
                    (lexp-append base-lxp (set-id set) (set-id child))
                    (lexp-make (set-id set) (set-id child))))
              (set-contents set)))
        (else (throw 'glean-type-error "SET-CHILD-LEXPS: Wrong type:" set))))

;;;;; Discipline lexp operations

;;;; Discipline Trees

(define-record-type <node>
  (make-node id children)
  node?
  (id       node-id)
  (children node-children))

(define (leaf? obj)
  (match obj
    (($ <node> id '()) #t)
    (_ #f)))

(define (serialize-node node)
  (match node
    (($ <node> id children)
     `(node (id ,id) (children ,@(map serialize-node children))))))

(define* (discipline-tree-base discipline node-id #:key modern?)
  "A higher-order function which, when provided with a DISCIPLINE and a
procedure identifying how to generate node-id's called NODE-ID, returns the
appropriate tree representation of discipline."
  (define (leaf id foundation set)
    (if modern?
        (make-node (node-id id foundation set) '())
        (cons (node-id id foundation set) '())))

  (define (node id foundation children set)
    (if modern?
        (make-node (node-id id foundation set)
                   (map (recurse-maker (cons id foundation)) children))
        (cons (node-id id foundation set)
              (map (recurse-maker (cons id foundation)) children))))

  (define (recurse-maker lxp-foundation)
    (lambda (set)
      (cond ((rootset? set)
             (leaf (set-id set) lxp-foundation set))
            (else
             (node (set-id set) lxp-foundation
                   (set-contents set) set)))))

  (if (plain-module? discipline)
      ((recurse-maker '()) discipline)
      (error "DISCIPLINE-TREE: DISCIPLINE is not a discipline:"
             discipline)))

(define* (discipline-serialized-tree discipline #:key modern?)
  "Return a tree representation of DISCIPLINE, where each component set is
replaced by a write friendly representation of the lexp identifying it.

Each entry is a pair: ((lexp path) . (list children))

Output should hence be:
 ((root) . (((root one) . '())
            ((root two) . ((root two-one . '())))))"
  (discipline-tree-base discipline
                        (lambda (id foundation set)
                          (reverse (cons id foundation)))
                        #:modern? modern?))

(define (discipline-tree->serialized lexp-tree)
  "Return a \"serialized\" version of LEXP-TREE, i.e. a version that can be
read as a plain list structure and which can be turned back into a full
lexp-tree using `discipline-serialized->tree.'"
  (match lexp-tree
    (((? lexp? lxp) . rest)
     (cons (lexp-serialize lxp)
           (map discipline-tree->serialized rest)))))

(define (discipline-serialized->tree serialized-tree)
  "Return a lexp-tree generated from the serialized lexp-tree
SERIALIZED-TREE.  This procedure is the counterpart to
`discipline-tree->serialized'."
  (match serialized-tree
    (((? list? full-lexp) . rest)
     (cons (lexp-make full-lexp)
           (map discipline-serialized->tree rest)))))

(define* (discipline-tree discipline #:key modern?)
  "Return a tree representation of DISCIPLINE, where each component set is
replaced by the lexp identifying it.

Each entry is a pair: (lexp . (list children))

Output should hence be:
 (lexp . ((lexp . '())
          (lexp . ((lexp . '())))))"
  (discipline-tree-base discipline
                        (lambda (id foundation set)
                          (lexp-make (reverse (cons id foundation))))
                        #:modern? modern?))

;;; lexp ends here
