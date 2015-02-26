;; set-tools.scm --- tests for set-tools    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 10 January 2015
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
;; Unit tests for set-tools.
;;
;; Source-file: glean/library/set-tools.scm
;;
;;; Code:

(define-module (tests set-tools)
  #:use-module (glean common hash)
  #:use-module (glean common utils)
  #:use-module (glean library lexp)
  #:use-module (glean library set-tools)
  #:use-module (glean library sets)
  #:use-module (ice-9 match)
  #:use-module (tests quickcheck-defs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  )


;;;; Tests

(test-begin "set-tools")

;;;;; Tests for: shallow-hash
;;;
;;; We want shallow hash to be different only if #:version, lxp or the lxps of
;;; a given set's children change.  Conversely, any other field can change to
;;; its heart's content — shallow-hash should still be string=?.

(test-assert "shallow-hash-equal"
  (let ((set ($mk-set)))
    (and
     (hash=? (shallow-hash (set-lexp set) set)
             (shallow-hash (set-lexp set) set))
     (hash=? (shallow-hash (set-lexp set) set)
             (shallow-hash
              (set-lexp set)
              ((compose (cut set-set-creator <> (string-reverse
                                                 (set-creator set)))
                        (cut set-set-description <> (string-reverse
                                                     (set-description set)))
                        (cut set-set-synopsis <> (string-reverse
                                                  (set-synopsis set)))
                        (cut set-set-keywords <> (reverse
                                                  (set-keywords set)))
                        (cut set-set-name <> (string-reverse
                                              (set-name set)))) set))))))

(test-assert "shallow-hash-not-equal"
  (let ((set ($mk-set 2 2)))
    (and
     (hash&!=? (shallow-hash (set-lexp set) set) ; lexp diff
               (shallow-hash (lexp (set target)) set))
     (hash&!=? (shallow-hash (set-lexp set) set) ; version diff
               (shallow-hash (set-lexp set)
                             (set-set-version
                              set (string-reverse (set-version set)))))
     (hash&!=? (shallow-hash (set-lexp set) set) ; child-lexp diff
               (shallow-hash (set-lexp set)
                             (set-set-contents set
                                               (cons ($mk-rootset)
                                                     (set-contents set))))))))

;;;;; Tests for: deep-hash
;;;
;;; We want deep hash only to be string=? to exactly a set that contains
;;; equal? values in each field.

;;; Are we the same if we deep-hash the same disc twice?
(test-assert "deep-hash-equal"
  (let ((disc ($mk-discipline 2 2)))
    (hash=? (deep-hash disc) (deep-hash disc))))

(define (ltrans l) (cons '(test) l))
(define (strans s) (string-append "test" s))
(define (lxptrans lxp) (if (lexp? lxp) (lexp-append lxp 'test) (lexp (test))))
(define proc-list (list `(,set-set-id ,set-id ,(cut symbol-append <> 'id))
                        `(,set-set-attribution ,set-attribution ,ltrans)
                        `(,set-set-contents ,set-contents ,ltrans)
                        `(,set-set-creator ,set-creator ,strans)
                        `(,set-set-description ,set-description ,strans)
                        `(,set-set-synopsis ,set-synopsis ,strans)
                        `(,set-set-keywords ,set-keywords ,ltrans)
                        `(,set-set-lineage ,set-lineage ,lxptrans)
                        `(,set-set-logo ,set-logo ,strans)
                        `(,set-set-name ,set-name ,strans)
                        `(,set-set-properties ,set-properties ,ltrans)
                        `(,set-set-attribution ,set-attribution ,ltrans)))

;;; Are we different if we change a field (at a time) in disc?
(test-assert "deep-hash-not-equal"
  (let ((disc ($mk-discipline 2 2)))
    (fold (lambda (procs result)
            (if result
                (match procs
                  ((setter accessor transformer)
                   (hash&!=? (deep-hash disc)
                             (deep-hash (setter disc (transformer
                                                      (accessor disc)))))))
                result))
          #t proc-list)))

;;; Are we different if we have the same disc, but change fields in the child?
(test-assert "deep-hash-not-equal-child"
  (let ((disc ($mk-discipline 2 2)))
    (fold (lambda (procs result)
            (if result
                (match procs
                  ((setter accessor transformer)
                   (hash&!=? (deep-hash disc)
                             (deep-hash
                              (set-set-contents
                               disc
                               `(,(car (set-contents disc))
                                 ,(setter (cadr (set-contents disc))
                                          (transformer
                                           (accessor
                                            (cadr
                                             (set-contents disc)))))))))))
                result))
          #t proc-list)))

;;;;; Tests for: dag-hash
;;;
;;; We want Dag-hash to equal as long as no new sets are added or removed in
;;; the discipline  or its children.
;;;
;;; Conversely, the hash should be different as soon as a set is added,
;;; removed, or moved to discipline or any of its children.

;;; Are we still the same if we change some fields?
(test-assert "dag-hash-equal"
  (let ((disc ($mk-discipline 2 2)))
    (and (hash=? (dag-hash disc) (dag-hash disc))
         (hash=? (dag-hash disc)
                 (dag-hash
                  ((compose (cut set-set-creator <> (string-reverse
                                                     (set-creator disc)))
                            (cut set-set-description <> (string-reverse
                                                         (set-description disc)))
                            (cut set-set-synopsis <> (string-reverse
                                                      (set-synopsis disc)))
                            (cut set-set-keywords <> (reverse
                                                      (set-keywords disc)))
                            (cut set-set-name <> (string-reverse
                                                  (set-name disc)))) disc))))))

;;; Are we still the same if we change some fields in the child?
(test-assert "dag-hash-equal-child"
  (let* ((disc ($mk-discipline 2 2))
         (chld (cadr (set-contents disc))))
    (and (hash=? (dag-hash disc) (dag-hash disc))
         (hash=?
          (dag-hash disc)
          (dag-hash 
           (set-set-contents
            disc
            `(,(car (set-contents disc))
              ,((compose
                 (cut set-set-creator <> (string-reverse
                                          (set-creator chld)))
                 (cut set-set-description <> (string-reverse
                                              (set-description chld)))
                 (cut set-set-synopsis <> (string-reverse
                                           (set-synopsis chld)))
                 (cut set-set-keywords <> (reverse
                                           (set-keywords chld)))
                 (cut set-set-name <> (string-reverse
                                       (set-name chld)))) chld))))))))

;;; Are we not equal if:
;;; - we add a new rootset
;;; - we remove a set
;;; - we add a new rootset to a child
;;; - we remove a set from a child
;;; - we exchange the 2 sets from the 2 children?
(test-assert "dag-hash-not-equal"
  (let* ((disc ($mk-discipline 2 2)))
    (match (set-contents disc)
      ((1st 2nd)
       (fold (lambda (new-disc result)
               (if result
                   (hash&!=? (dag-hash disc) (dag-hash new-disc))
                   result))
             #t
             (list
              (set-set-contents disc (cons ($mk-rootset) (set-contents disc)))
              (set-set-contents disc `(,2nd))
              (set-set-contents disc `(,(set-set-contents
                                         1st (cons ($mk-rootset)
                                                   (set-contents 1st)))
                                       ,2nd))
              (set-set-contents disc `(,(set-set-contents
                                         1st (cdr (set-contents 1st)))
                                       ,2nd))
              (match (cons (set-contents 1st) (set-contents 2nd))
                (((1st-1st . rest-1st) . (1st-2nd . rest-2nd))
                 (set-set-contents disc `(,(set-set-contents
                                            1st (cons 1st-2nd rest-1st))
                                          ,(set-set-contents
                                            2nd (cons 1st-1st
                                                      rest-2nd))))))))))))
;;;;; Tests for: discipline-ancestry-tree

(define test-ancestor
  (module 'root
      #:contents `(,(set 'one
                         #:contents `(,(set 'one-one)))
                   ,(set 'two
                         #:contents `(,(problem (q "test")
                                                (s "test"))))
                   ,(set 'three
                         #:contents `(,(tutorial 'three-one)
                                      ,(set 'three-two))))))

(define test-module
  (module 'root
      ;; some of root's children have moved and have had their ids changed ->
      ;; new shallow-hash
      #:contents `(
                   ;; This set's id has been changed -> new shallow-hash.
                   ,(set 'four
                         #:lineage (lexp (root one))
                         #:contents
                         `(
                           ;; this set's id has been changed -> new shallow-hash
                           ,(set 'one-four
                                 #:lineage (lexp (root one one-one)))))
                   ;; this subtree remains identical -> (#f . hash) for all.
                   ,(set 'two
                         #:contents `(,(problem (q "test")
                                                (s "test"))))
                   ;; This set's id has been changed -> new shallow-hash.
                   ,(set 'seven
                         #:lineage (lexp (root three))
                         ;; these are essentially new sets (no lineage to link
                         ;; to previous) -> ("" . hash) for both.
                         #:contents `(,(tutorial 'seven-one)
                                      ,(set 'seven-two))))))
(define test-module-false
  (module 'root
    #:contents `(,(set 'four
                       ;; This should trigger lexp resolution problems.
                       #:lineage (lexp (root unknown))
                       #:contents `(,(set 'one-four)))
                 ,(set 'two
                       #:contents `(,(problem (q "test")
                                              (s "test"))))
                 ,(set 'seven
                       ;; This too.
                       #:lineage (lexp (root blah))
                       #:contents `(,(tutorial 'seven-one)
                                    ,(set 'seven-two))))))

;;; This is the usual scenario: some subsets of a DISCIPLINE contain lexps
;;; that resolve to their direct relations in ANCESTOR.  This being the case,
;;; those subsets, and any direct parents of such will have a mapping from
;;; ANCESTOR -> DISCIPLINE.
;;; FIXME: as we are using no inheritance (and as inheritance of lineage is
;;; not yet implemented in core-templates, we must either specify the lineage
;;; paths for alll subsets or expect "" rather than #f.)
(test-assert "discipline-ancestry-tree"
  (match (discipline-ancestry-tree test-module test-ancestor)
    (
     (("gdigdhz4r6n4fih6rsgcjorqqhahvxv4i57ondemq4wsz7jenduq"
       . "hfg7shivdryi7zvcm73gq3dwnnvjw2mbfeg7dqrce66rvlqx4yoa")
      (("6356y5sz5njak5d2sg5tz3ydx2gto23ensuroivjqdxdskq4ydua"
        . "kum54szklilnuouk2ptpfwlu6q7lz4djjuczh6jbsfigbz6cfqfa")
       (("wjq3zaldphyakhomojrtoq3pi3ongwypn4vu3bca2qfdmgq2tvlq"
         . "4gttoxgr354ty2e5fjezmguuor32xr76jezki34ijj7f7psienpq")))
      ((#f . "nxr2ofrijjleetfnplyqjpols5y63viafulrpel2x23xjtfvomnq"))
      (("vd5t3awvrjulipmsndlmqfihwi3anquok6xbbyo6wytxohcu7m3a"
        . "bjxqhdjzd6uhezz6fvowu7qlitlt7oaukv22tjgff6hcywgxduna")
       (("" . "i3g7uge6ebyjmbt7px2moiezatyrujii3jgmg5yczpf5c7agdntq"))
       (("" . "5wsdwho6enzlc4fgc4knxcjxddmffpvl5rjfah4p6wz2lbpya75a"))))
     #t)
    (otherwise otherwise)))

;;; We're testing what happens when DISCIPLINE contains lineage fields that
;;; have unresolvable lexps with regards to ANCESTOR.
(test-assert "discipline-ancestry-tree-unresolvable"
  (match (discipline-ancestry-tree test-module-false test-ancestor)
    (
     (("gdigdhz4r6n4fih6rsgcjorqqhahvxv4i57ondemq4wsz7jenduq"
       . "hfg7shivdryi7zvcm73gq3dwnnvjw2mbfeg7dqrce66rvlqx4yoa")
      ((($ <nothing> 'problematic-set-lineage)
        . "kum54szklilnuouk2ptpfwlu6q7lz4djjuczh6jbsfigbz6cfqfa")
       ((#f . "4gttoxgr354ty2e5fjezmguuor32xr76jezki34ijj7f7psienpq")))
      ((#f . "nxr2ofrijjleetfnplyqjpols5y63viafulrpel2x23xjtfvomnq"))
      ((($ <nothing> 'problematic-set-lineage)
        . "bjxqhdjzd6uhezz6fvowu7qlitlt7oaukv22tjgff6hcywgxduna")
       ((#f . "i3g7uge6ebyjmbt7px2moiezatyrujii3jgmg5yczpf5c7agdntq"))
       ((#f . "5wsdwho6enzlc4fgc4knxcjxddmffpvl5rjfah4p6wz2lbpya75a"))))
     #t)
    (otherwise otherwise)))

;;; More generally:

(test-assert "discipline-ancestry-tree-random-new-child"
  ;; A random scenario in which we have added a new rootset to the first child
  ;; of the discipline.
  ;; - The crownhash should have a new hash (because we have a new child).
  ;; - The first child of the discipline is new ("" . hash).
  ;; - All else should be unchanged (#f . hash).
  (let* ((ancestor ($mk-discipline 2 2))
         (discipline (set-set-contents ancestor
                                       (cons ($mk-rootset)
                                             (set-contents ancestor)))))
    (match (discipline-ancestry-tree discipline ancestor)
      (
       ((? hash&!=?)
        ((? new.hash?))
        ((? false.hash?)
         ((? false.hash?))
         ((? false.hash?)))
        ((? false.hash?)
         ((? false.hash?))
         ((? false.hash?))))
       #t)
      (otherwise otherwise))))

(test-assert "discipline-ancestry-tree-random-switch-order"
  ;; A random scenario in which we simply reversed the order of the ancestor's
  ;; children.
  ;; - The crownhash should have a new hash (because the order of set-contents
  ;;   has changed)
  ;; - All else should remain identical (#f . hash) no other structure has
  ;;   changed at all.
  (let* ((ancestor ($mk-discipline 2 2))
         (discipline (set-set-contents ancestor
                                       `(,(cadr (set-contents ancestor))
                                         ,(car (set-contents ancestor))))))

    (match (discipline-ancestry-tree discipline ancestor)
      (
       ((? hash&!=?)
        ((? false.hash?)
         ((? false.hash?))
         ((? false.hash?)))
        ((? false.hash?)
         ((? false.hash?))
         ((? false.hash?))))
       #t)
      (otherwise #f))))

(test-assert "discipline-ancestry-tree-random-switch-order-add-child"
  ;; A random scenario in which we reversed the order of the ancestor's
  ;; children, and added a child to the first child of discipline.
  ;; - The crownhash should have a new hash (because the order of set-contents
  ;;   hash changed),
  ;; - The first child should have a new hash (because a new child was added)
  ;; - The first child of the first child should have ("" . hash), because it
  ;;   is new.
  ;; - All others should be (#f . hash)
  (let* ((ancestor ($mk-discipline 2 2))
         (discipline (set-set-contents
                      ancestor
                      `(,(set-set-contents
                          (cadr (set-contents ancestor))
                          (cons ($mk-rootset)
                                (set-contents
                                 (cadr (set-contents ancestor)))))
                        ,(car (set-contents ancestor))))))

    (match (discipline-ancestry-tree discipline ancestor)
      (
       ((? hash&!=?)
        ((? hash&!=?)
         ((? new.hash?))
         ((? false.hash?))
         ((? false.hash?)))
        ((? false.hash?)
         ((? false.hash?))
         ((? false.hash?))))
       #t)
      (otherwise #f))))

(test-assert "discipline-ancestry-tree-random-cut-child"
  ;; A random scenario in which we remove the first child of the first child
  ;; of discipline.
  ;; - The crownhash should remain unchanged (#f . hash).
  ;; - The first child should have a new hash (because a child was removed).
  ;; - All others should be (#f . hash)
  (let* ((ancestor ($mk-discipline 2 2))
         (discipline (set-set-contents
                      ancestor
                      (cons (set-set-contents
                             (car (set-contents ancestor))
                             (cdr (set-contents
                                   (car (set-contents ancestor)))))
                            (cdr (set-contents ancestor))))))

    (match (discipline-ancestry-tree discipline ancestor)
      (
       ((? false.hash?)
        ((? hash&!=?)
         ((? false.hash?)))
        ((? false.hash?)
         ((? false.hash?))
         ((? false.hash?))))
       #t)
      (otherwise #f))))

;;;;; The reason we introduced set-lineage:
;;;
;;; In the following scenarios we simply want to move a child from the first
;;; child in discipline to the second child in discipline.
;;;
;;; Without using lineage (the first test), Glean will conclude that the
;;; first child of discipline has lost a child, and the second discipline has
;;; gained a new child.
;;;
;;; This is obviously undesirable: the child has simply moved, so all stats
;;; associated with that child should be retained.  See the next test for the
;;; solution to this.

(test-assert "discipline-ancestry-tree-random-move-child"
  ;; A random scenario in which we move the first child of the first child to
  ;; the beginning of the children of the second child of discipline.
  ;; - The crownhash should remain unchanged (#f . hash).
  ;; - The first child should have a new hash (because a child was removed).
  ;; - The second child should have a new hash (a child was added).
  ;; - The first child of the second child should have ("" . hash): it's new.
  ;; - All others should be (#f . hash)
  (let* ((ancestor ($mk-discipline 2 2))
         (discipline (set-set-contents
                      ancestor
                      `(,(set-set-contents
                          (car (set-contents ancestor))
                          (cdr (set-contents
                                (car (set-contents ancestor)))))
                        ,(set-set-contents
                          (cadr (set-contents ancestor))
                          (cons (car (set-contents
                                      (car (set-contents ancestor))))
                                (set-contents
                                 (cadr (set-contents ancestor)))))))))

    (match (discipline-ancestry-tree discipline ancestor)
      (
       ((? false.hash?)
        ((? hash&!=?)
         ((? false.hash?)))
        ((? hash&!=?)
         ((? new.hash?))
         ((? false.hash?))
         ((? false.hash?))))
       #t)
      (otherwise #f))))

(test-assert "discipline-ancestry-tree-random-move-child-linked"
  ;; A random scenario in which we move the first child of the first child to
  ;; the beginning of the children of the second child of discipline.  In this
  ;; case we use lineage to track the transfer.
  ;; - The crownhash should remain unchanged (#f . hash).
  ;; - The first child should have a new hash (because a child was removed).
  ;; - The second child should have a new hash (a child was added).
  ;; - The first child of the second child should have:
  ;;   `(,(shallow-hash (first-child-first-child)) . hash)
  ;; - All others should be (#f . hash)
  (let* ((ancestor ($mk-discipline 2 2))
         (lxp (lexp-make (list (set-id ancestor)
                               (set-id (car (set-contents ancestor)))
                               (set-id (car (set-contents
                                             (car (set-contents
                                                   ancestor))))))))
         (discipline (set-set-contents
                      ancestor
                      `(,(set-set-contents
                          (car (set-contents ancestor))
                          (cdr (set-contents
                                (car (set-contents ancestor)))))
                        ,(set-set-contents
                          (cadr (set-contents ancestor))
                          (cons (set-set-lineage
                                 (car (set-contents
                                       (car (set-contents ancestor))))
                                 lxp)
                                (set-contents
                                 (cadr (set-contents ancestor)))))))))

    (match (discipline-ancestry-tree discipline ancestor)
      (
       ((? false.hash?)
        ((? hash&!=?)
         ((? false.hash?)))
        ((? hash&!=?)
         ((? (lambda (pair)
               (match pair
                 ((a . b)
                  (and (hash=? a
                               (shallow-hash lxp
                                             (lexp-set-resolve ancestor lxp)))
                       (string? b)))
                 (_ #f)))))
         ((? false.hash?))
         ((? false.hash?))))
       #t)
      (otherwise #f))))

;;;;; Tests for: hashtree?

(test-assert "hashtree-true"
  (hashtree? '(("hash" . (('prop . test))) ())))

(test-assert "hashtree-deeper-true"
  (hashtree? '(("hash" . (test)) ((("hash" . (test)))))))

(test-assert "hashtree-false"
  (not (hashtree? '(blah blah))))

;;;;; Tests for: make-hashtree

(test-assert "make-hashtree"
  (let ((disc ($mk-rootset 10)))
    (hashtree? (make-hashtree disc (lexp-make (set-id disc))))))

(test-assert "make-deep-hashtree"
  (let ((disc ($mk-discipline 3 3)))
    (hashtree? (make-hashtree disc (lexp-make (set-id disc))))))

;;;;; Tests for: hashmap?

(test-assert "hashmap-true?"
  (let ((set ($mk-discipline)))
    (hashmap? `((test) "hash" ,(make-hashtree set (lexp-make (set-id set)))))))

;;;;; Tests for: make-hashmap

(test-assert "make-hashmap"
  (hashmap? (make-hashmap ($mk-discipline 3 3))))

(test-end "set-tools")

;;; set-tools ends here
