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
  #:use-module (glean library sets)
  #:use-module (glean library set-tools)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export     (derive-upgrade-map
                serialize-upgrade-map

                <upgrade-map>
                make-upgrade-map
                upgrade-map?
                upgrade-map-dag
                upgrade-map-map
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

(define* (inspect-upgrade-map upgrade-map #:key (port #t))
  "A detailed printer for upgrade-maps."
  (match upgrade-map
    (($ <upgrade-map> current-set old-dag generations map)
     (format port "Upgrade Map For '~a'~%Upgrading from '~a' over ~a generations.~%"
             (set-name current-set) old-dag generations)
     ;; Should recursively pretty print the map
     (format port "  ~a~%" map))))

;;;;; For sheer testing
(define tset
  ;; extract git from store
  ((@ (glean library library-store) fetch-set-from-lexp)
   ((@ (glean library lexp) lexp) git)
   ((@ (glean library library-store) catalogue-hash)
    (@ (glean config) %current-catalogue%))))


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
  `(upgrade-map (dag-hash    ,(upgrade-map-dag map))
                (generations ,(upgrade-map-generations map))
                (map         ,(upgrade-map-map map))
                (set         ,(make-hashmap (upgrade-map-set map)))))

;;; <set> dag-hash -> <upgrade-map> | error
(define (derive-upgrade-map set dag)
  "Return the upgrade-map leading from the previous version of SET to SET
identified by DAG."
  (match (select-ancestry-tree (set-ancestry set) dag)
    ;; Simple case: the first ancestry tree is the one we want.
    ;; As DAG was matched after crossing 0 generations, the first entry will
    ;; carry information how to upgrade from DAG to current version.
    ((0 . tree)
     (inform (_ "We have a simple upgrade scenario.~%"))
     (make-upgrade-map set dag 0 tree))
    ;; Complicated: we have to cross several generations, and must build a
    ;; composite upgrade tree.
    ((generations . tree)
     (insist (_ "We have not implemented multi-generation upgrades yet.~%"))
     (make-upgrade-map set dag generations tree))
    (otherwise
     (throw 'glean-logic-error
            "DERIVE-UPGRADE-MAP: unexpected format:" otherwise))))

;;; ((dag . tree) ...) <string dag-hash> -> (<int generations> . tree)
(define (select-ancestry-tree trees dag)
  "Return a pair consisting of a 0 indexed generation counter and the tree
that we want to upgrade from, selected from TREES, using DAG as the tree of
interest."
  (fold (lambda (ancestry-tree previous)
          (match previous
            ((generations . tree) previous)
            (generations
             (match ancestry-tree
               (((? (cute string=? <> dag)) . tree) (cons generations tree))
               (otherwise                           (1+ generations))))))
        0 trees))

;;; set-operations ends here

;;;; Pensées
;;;
;;; - Compile core-template sets should end up with a lexp field, as part of
;;;   the final set.  This would allow us to immediately extract context from
;;;   the set, anywhere in Glean, significantly simplifying many operations.
