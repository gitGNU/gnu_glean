;; set-operations.scm --- tests for set-operations    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 18 May 2015
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
;; Unit tests for set-operations.
;;
;; Source-file: glean/library/set-operations.scm
;;
;;; Code:

(define-module (tests set-operations)
  #:use-module (glean library set-operations)
  #:use-module (glean library set-tools)
  #:use-module (glean library sets)
  #:use-module (tests quickcheck-defs)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  )

(define select-ancestry-tree
  (@@ (glean library set-operations) select-ancestry-tree))

(define expand-ancestry-tree
  (@@ (glean library set-operations) expand-ancestry-tree))

;;;;; For sheer testing
;;; This will only work if the Git Discipline is installed in the library.
;;; It is here for interactive testing of upgrade-map output.
(define test-set
  ;; extract git from store
  ((@ (glean library library-store) fetch-set-from-lexp)
   ((@ (glean library lexp) lexp) git)
   ((@ (glean library library-store) catalogue-hash)
    (@ (glean config) %current-catalogue%))))
(define test-dag "geixkimhdpfpmqiueqtbisq5ddwokt6gaqalq6g333lqsxsnjz7q")
(define test-upgrade-map (derive-upgrade-map test-set test-dag))


;;;; Tests

(test-begin "set-operations")

;;;;; Tests for: select-ancestry-tree

(test-assert "select-ancestry-tree"
  (let ((trees '(("foo" . (foo-tree))
                 ("bar" . (bar-tree)))))
    (and (match (select-ancestry-tree trees "bar")
           ((1 . ('bar-tree)) #t)
           (_                 #f))
         (match (select-ancestry-tree trees "foo")
           ((0 . ('foo-tree)) #t)
           (_                 #f))
         (match (select-ancestry-tree trees "blah")
           ((num . #f) #t)
           (_          #f)))))

;;;;; Tests for: expand-ancestry-tree

(test-assert "expand-ancestry-tree"
  (let* ((origin        ($mk-discipline #:children 2 #:depth 2))
         (ancestor      (set-set-contents origin (cdr (set-contents origin))))
         (ancestry-tree (discipline-ancestry-tree origin ancestor))
         (dag           (dag-hash ancestor))
         (set           (set-set-ancestry origin `((,dag . ,ancestry-tree))))
         (hashmap       (make-hashmap set #:labels? #t))
         (hashtree      (cadr (assq 'hashtree hashmap)))
         (expanded      (expand-ancestry-tree ancestry-tree hashtree)))
    ;; By deduction, the root entry of the upgrade map will be an 'update
    ;; (from ancestor to set, we introduced a new subset).
    (match expanded
      ((('update ('original hash)
                 (? (lambda (_) (equal? _ (assq 'shallow-hash hashtree))))
                 (? (lambda (_) (equal? _ (assq 'properties hashtree))))) . _)
       #t)
      (_ #f))))

;;;;; Tests for: derive-upgrade-map

(test-assert "derive-upgrade-map"
  (let* ((origin        ($mk-discipline #:children 2 #:depth 2))
         (ancestor      (set-set-contents origin (cdr (set-contents origin))))
         (ancestry-tree (discipline-ancestry-tree origin ancestor))
         (dag           (dag-hash ancestor))
         (set           (set-set-ancestry origin `((,dag . ,ancestry-tree))))
         (upgrade-map   (derive-upgrade-map set dag)))
    (and (equal? `((old ,dag) (new ,(dag-hash set)))
                 (upgrade-map-dag upgrade-map))
         (equal? (upgrade-map-set upgrade-map) set)
         (=      (upgrade-map-generations upgrade-map) 0))))

(test-end "set-operations")

;;; set-operations ends here
