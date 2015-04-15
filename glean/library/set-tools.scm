;; set-tools.scm --- intermediate set functionality    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 07 January 2014
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
;; This module exposes intermediate procedures for set manipulation.  The
;; procedures are not part of (glean library sets) because they have higher
;; level dependencies.
;;
;;; Code:

(define-module (glean library set-tools)
  #:use-module (glean common hash)
  #:use-module (glean common utils)
  #:use-module (glean library sets)
  #:use-module (glean library lexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export     (dag-hash
                deep-hash
                discipline-ancestry-tree
                hashmap?
                hashtree?
                make-hashmap
                make-hashtree
                shallow-hash))


;;;; Discipline Hashes
;;;
;;; Procedures in this section define hashes used in Glean's handling of
;;; disciplines.  Hashes are used, at variable levels of precision, to
;;; identify or refer to specific (versions of) disciplines.

(define (shallow-hash lxp set)
  "Return the hash of the string concatenation of lexp LXP, SET's version
string, and the list of lexps of SET's immediate child sets.

If SET contains problems rather than further sets, the list of lexps will be
the empty list.

This hash provides a compromise: it is flexible enough to remain constant over
tweaks to metadata of the set identified by LXP and its children, but it will
register changes to the structure of LXP's children, and indeed to version
bumps for the set or LXP changes.

The primary use for this hash is in the provision of 2 types of trees for the
lounge: hashtrees when disciplines are first enabled, and ancestrytrees when
already known disciplines need to be shifted to use a new structure."
  (if (and (lexp? lxp) (set? set))
      (sha256-string-strict
       (string-join `(,(object->string lxp) ,(set-version set)
                      ,(object->string (set-child-lexps set lxp)))
                    "-"))
      (_ (throw 'glean-type-error "SHALLOW-HASH: incorrect types" lxp set))))

(define (deep-hash discipline)
  "Return the hash which is the result of recursively turning each element in
DISCIPLINE-FIELDS into a string, concatenating and finally hashing it.

This hash provides an unambiguous identifier for a revision of the slightest
form:  any field modified in a discipline's definition will result in a
different hash.

This hash's primary use is as a unique identifier for installing a discipline
revision in the library's store.  This approach should allow us to
non-destructively install, roll-back to and/or concurrently use multiple
revisions of the same discipline."
  ;; the simplicity of this hash relies on glean library records being srfi-9
  ;; records, which means that object->string reveals all data for them.
  (match discipline
    ((? plain-module? discipline)
     (sha256-string-strict (object->string discipline)))
    (otherwise (throw 'glean-type-error "DEEP-HASH: Wrong type:" discipline))))


(define (dag-hash discipline)
  "Return the hash summarizing the dag represented by LEXP-TREE.  LEXP-TREE in
turn represents an entire discipline's abstract structure (the relation
between each contained set, it's parent, and it's children).

dag-hashes provide a snapshot of a discipline's entire tree structure.  A
comparison between two dag-hashes will tell you with absolute certainty
whether the structure of either of the two disciplines is different.
Conversely, one can use a dag-hash to pinpoint the exact \"structural\"
revision of a discipline.

The use of this hash is as follows:
- a discipline's lineage field is populated by a dictionary, with as keys
  dag-hashes and as values ancestry-trees.
- the first entry in the lineage field is
  ((dag-hash current-version) . (ancestry-tree current previous))
- every challenge request will contain four values:
  + a counter for selecting a problem within a rootset,
  + the shallow-hash of the challenge requested,
  + the base-lexp of the discipline containing the challenge,
  + the dag-hash of the revision of the discipline containing the challenge.

When the library receives such a request, it will:
- match the challenge shallow-hash, and return the challenge;
- OR match the base-lexp against its currently registered base-lexps:
  + if not found: return not found error to lounge
  + if found:
    * revisit the discipline's lineage field,
    * match the entry with the request's dag-hash,
    * construct an upgrade path from that entry to the latest version,
    * return this upgrade path to the lounge,

This approach should guarantee robust upgrade (and downgrade) paths for the
lounge, unique entries in a lounge's database for each set in any discipline,
unambiguous addressing between the lounge and the library."
  (sha256-string-strict (object->string (discipline-tree discipline))))


;;;; Hash using procedures

(define (discipline-ancestry-tree discipline ancestor-discipline)
  "Return a tree providing a schema by which an ancestor lexp-tree could be
modified to the equivalent tree for discipline.

Ancestry trees always operate on serialized trees, as their primary function
relates to their being read/written to/from files and passed to the lounge.

Finally, the hashes we use here are shallow hashes: they capture changes to
the structure of the discipline and version changes.  The hashing procedure
that will be used is set-shallow-hash (a hash of the set's lexp, each direct
child lexp, and its version).  For now we use set-fullhash (in the long term
this is undesirable as it will force lounge updates on a far to frequent
basis).

The format is as follows:
 ((ancestor-hash . disc-hash) . ((ancestor-hash . disc-hash) ...))

The resulting tree may contain nothings of id 'lexp-unknown, if some lexp's in
DISCIPLINE did not resolve to sets in ANCESTOR-DISCIPLINE."
  (let ((resolve-ancestor (cut lexp-set-resolve ancestor-discipline <>)))

    (define (node-id id foundation set)
      ;; We must provide ancestor's shallow-hash, if ...
      (let* ((lxp    (lexp-make (reverse (cons id foundation))))
             (hasher (cut shallow-hash lxp <>))
             (hash   (hasher set)))

        (cond ((set-lineage set)     ; ... set has set-lineage
               (match (resolve-ancestor (set-lineage set))
                 ((? set? ancestor)
                  `(,(shallow-hash (set-lineage set) ancestor) . ,hash))
                 (($ <nothing> 'lexp-unknown context)
                  `(,(nothing 'problematic-set-lineage context) . ,hash))))

              (else (match (resolve-ancestor lxp)
                      (($ <nothing> 'lexp-unknown) ; new subset
                       `("" . ,hash))
                      ((? (compose (cut hash=? <> hash)
                                   (cut hasher <>))) ; no change
                       `(#f . ,hash))
                      (ancestor ; subset, or its children, changed
                       `(,(hasher ancestor) . ,hash)))))))

    (discipline-tree-base discipline node-id)))


;;;; Hashtrees and Hashmaps: abstract representations for the lounge.

(define (hashtree? obj)
  "return #t if obj is a hashtree, #f otherwise.

a hashtree is a list with a pair of the form '(blobhash . properties) as its
car, and either '() or a list containing hashtrees as its cdr."
  (match obj
    ((((? hash?) . (? list?)) ((? hashtree?) ...)) #t)
    ((((? hash?) . (? list?))) #t)
    (_ #f)))

(define (make-hashtree set lxp)
  "return a hashtree, starting with set, and accumulated lexp lxp."
  (define (hashtree set)
    (make-hashtree set (lexp-append lxp (set-id set))))
  (cond ((rootset? set)
         (list (cons (shallow-hash lxp set)
                     (set-properties set))))
        (else (list (cons (shallow-hash lxp set)
                          (set-properties set))
                    (map hashtree (set-contents set))))))

(define (hashmap? obj)
  "return #t if obj is a hashmap, #f otherwise.

a hashmap is a hashtree of a discipline, prepended by the discipline's
base-lexp, and its dag-hash."
  (match obj
    (((base-id) (? hash?) (? hashtree?)) #t)
    (_ #f)))

(define (make-hashmap discipline)
  "return a hashmap of discipline."
  (let ((lxp (lexp-make (set-id discipline))))
    (list (lexp-serialize lxp) (dag-hash discipline)
          (make-hashtree discipline lxp))))

;;; set-tools ends here
