;; set-operations.scm --- high-level set operations    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 9 May 2015
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
;; A module providing set level operations.  Conceptually this would sit right
;; beneath the server and/or library store, but on top of set-tools, and sets.
;;
;;; Code:

(define-module (glean library set-operations)
  #:use-module (glean common utils)
  #:use-module (glean library lexp)
  #:use-module (glean library sets)
  #:use-module (glean library set-tools)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export     (derive-upgrade-map
                serialize-upgrade-map

                <upgrade-map>
                make-upgrade-map
                inspect-upgrade-map
                upgrade-map?
                upgrade-map-dag
                upgrade-map-generations
                upgrade-map-map
                upgrade-map-set
                ))


;;;; Record Definitions
;;;
;;; Records used in this context

(define-record-type <upgrade-map>
  (make-upgrade-map current-set old-dag generations map)
  upgrade-map?
  (current-set upgrade-map-set)
  (old-dag upgrade-map-dag)
  (generations upgrade-map-generations)
  (map upgrade-map-map))

(set-record-type-printer! <upgrade-map>
  (lambda (record port)
    (match record
      (($ <upgrade-map> current-set old-dag generations map)
       (format
        port
        "<upgrade-map current-set: '~a'> [introspection: inspect-upgrade-map]"
        (set-id current-set))))))

(define* (inspect-upgrade-map upgrade-map #:key (port #t) (serialized? #t))
  "A detailed printer for upgrade-maps."
  (if serialized?
      (pretty-print (serialize-upgrade-map upgrade-map))
      (match upgrade-map
        (($ <upgrade-map> current-set (('old old) ('new new)) generations map)
         (format port "Upgrade Map For '~a'~%\
Upgrading from '~a' to '~a' over ~a generations.~%"
                 (set-name current-set) old new generations)
         ;; Should recursively pretty print the map
         (format port "~a~%" (pretty-print map ))))))


;;;; Porcelain Procedures
;;;
;;; Procedures to be exported

;;; <upgrade-map> -> (('dag-hash . <string hash>)
;;;                   ('generations . <int generations>) ('map . (map))
;;;                   ('set . (hashmap)))
(define (serialize-upgrade-map map)
  "Return a serialized version of the upgrade-map MAP, providing all required
data for a lounge to upgrade using MAP."
  ;; This format below is compatible with sxml.  HASHMAPS and UPGRADE-MAPS
  ;; are not yet.
  `(upgrade-map (lexp        ,(lexp-serialize
                               (lexp-make (set-id (upgrade-map-set map)))))
                (dag-hash    ,@(upgrade-map-dag map))
                (generations ,(upgrade-map-generations map))
                (map         ,(upgrade-map-map map))))

;;; <set> dag-hash -> <upgrade-map> | error
(define (derive-upgrade-map set dag)
  "Return the upgrade-map leading from the previous version of SET to SET
identified by DAG."
  (match (make-hashmap set #:labels? #t)
    ((('lexp lxp) ('dag-hash new-dag) ('hashtree htree))
     (match (select-ancestry-tree (set-ancestry set) dag)
       ((generations . #f)
        (throw 'glean-library-error
               "DERIVE-UPGRADE-MAP: DAG unknown in set's ancestry tree."
               dag (set-name set)))
       ;; Simple case: the first ancestry tree is the one we want.
       ;; As DAG was matched after crossing 0 generations, the first entry will
       ;; carry information how to upgrade from DAG to current version.
       ((0 . utree)
        (inform (_ "We have a simple upgrade scenario.~%"))
        (make-upgrade-map set `((old ,dag) (new ,new-dag)) 0
                          (expand-ancestry-tree utree htree)))
       ;; Complicated: we have to cross several generations, and must build a
       ;; composite upgrade tree.
       ((generations . utree)
        (insist (_ "We have not implemented multi-generation upgrades yet.~%"))
        (throw 'glean-fixme-error "DERIVE-UPGRADE-MAP: not supported."))
       (otherwise
        (throw 'glean-logic-error
               "DERIVE-UPGRADE-MAP: unexpected format:" otherwise))))))

;;; ((dag . tree) ...) <string dag-hash> -> (<int generations> . tree)
(define (select-ancestry-tree trees dag)
  "Return a pair consisting of a 0 indexed generation counter and the tree
that we want to upgrade from, selected from TREES, using DAG as the tree of
interest."
  (if trees
      (fold (lambda (ancestry-tree previous)
              (match previous
                ((generations . #f)
                 (match ancestry-tree
                   (((? (cute string=? <> dag)) . tree)
                    (cons generations tree))
                   (otherwise `(,(1+ generations) . #f))))
                (otherwise previous)))
            `(0 . #f) trees)
      (throw 'glean-type-error
             "SELECT-ANCESTRY-TREE: We have no acnestry tree.")))

;; (ancestry tree) (hashtree #:labels? #t) -> (expanded ancestry tree)
(define (expand-ancestry-tree ancestrytree hashtree)
  "Return a new tree, the result of expanding ANCESTRYTREE with the nodes &
leaves of HASHMAP."
  (define (index-hashtree hashtree vhash)
    "Return vhash augmented by the nodes and leaves in hashtree.  Their
shallow-hashes will be used as the keys for vhash."
    (match hashtree
      ((('shallow-hash hash) ('properties props))
       (vhash-cons hash `((shallow-hash ,hash) (properties ,props)) vhash))
      ((('shallow-hash hash) ('properties props) ('child-hashtrees children))
       (fold index-hashtree
             (vhash-cons hash `((shallow-hash ,hash) (properties ,props)) vhash)
             children))))
  (define (expand ancestrytree)
    "Return a new ancestry tree, built from ANCESTRYTREE, with each node or
leaf their new shallow-hash replaced with the expanded entry from index."
    (match ancestrytree
      ((('update oldh newh) . children)
       `((update (original ,oldh) ,@(cdr (vhash-assoc newh index)))
         . ,(map expand children)))
      (((keyword hash) . children)
       `((,keyword ,@(cdr (vhash-assoc hash index)))
         . ,(map expand children)))))
  (define index (index-hashtree hashtree vlist-null))

  (expand ancestrytree))

;;; set-operations ends here

;;;; Pensées
;;;
;;; - Compile core-template sets should end up with a lexp field, as part of
;;;   the final set.  This would allow us to immediately extract context from
;;;   the set, anywhere in Glean, significantly simplifying many operations.
