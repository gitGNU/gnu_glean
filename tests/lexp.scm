;; lexp.scm --- tests for lexp    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 26 December 2014
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
;; Unit tests for lexp.
;;
;; Source-file: glean/library/lexp.scm
;;
;;; Code:

(define-module (tests lexp)
  #:use-module (glean common utils)
  #:use-module (glean library lexp)
  #:use-module (glean library sets)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (tests quickcheck-defs))


;;;; Definitions / Mock

(define test-set (set 'test #:contents `(,(set 'child) ,(set 'second)
                                         ,(set 'third))))

;; Changing the mock data structures will require tweakes to the tree tests.
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
    #:contents `(,(set 'four
                       #:lineage (lexp (root one))
                       #:contents `(,(set 'one-four)))
                 ,(set 'two
                       #:contents `(,(problem (q "test")
                                              (s "test"))))
                 ,(set 'seven
                       #:lineage (lexp (root three))
                       #:contents `(,(tutorial 'seven-one)
                                    ,(set 'seven-two))))))
(define test-module-false
  (module 'root
    #:contents `(,(set 'four
                       #:lineage (lexp (root unknown))
                       #:contents `(,(set 'one-four)))
                 ,(set 'two
                       #:contents `(,(problem (q "test")
                                              (s "test"))))
                 ,(set 'seven
                       #:lineage (lexp (root blah))
                       #:contents `(,(tutorial 'seven-one)
                                    ,(set 'seven-two))))))

(define lexp-make (@@ (glean library lexp) lexp-make))
(define <lexp> (@@ (glean library lexp) <lexp>))

;;;; Tests

(test-begin "lexp")

;;;;; Basic lexp tests

(test-assert "lexp?"
  (lexp? ((@@ (glean library lexp) lexp-mecha-make) 'test '(rest))))

;;;;; Lexp make

(test-assert "lexp-make"
  (and (match (lexp-make 'test 'one 'two)
         (($ <lexp> 'test (one two)) #t))
       (match (lexp-make '(test one two))
         (($ <lexp> 'test (one two)) #t))))

(test-assert "lexp-list"
  (let ((lxp (lexp-make '(hello scheme world))))
    (and (eqv? (lexp-base lxp) 'hello)
         (equal? (lexp-rest lxp) '(scheme world)))))

(test-assert "lexp-symbols"
  (let ((lxp (lexp-make 'hello 'scheme 'world)))
    (and (eqv? (lexp-base lxp) 'hello)
         (equal? (lexp-rest lxp) '(scheme world)))))

;;;;; Other

(test-assert "lexp-base"
  (fold (lambda (lxp prev)
          (and prev
               (eqv? (lexp-base lxp) 'hello)
               (equal? (lexp-rest lxp) '())))
        #t
        (list (lexp-make 'hello) (lexp-make '(hello)))))

(test-equal "lexp-serialize"
  (lexp-serialize (lexp (test one))) '(test one))

(test-equal "lexp-append"
  (lexp-append (lexp (test one)) 'two 'three)
  (lexp (test one two three)))

;;;;; LEXP member

(test-eqv "lexp-member-third"
  (set-id (lexp-set-member test-set (lexp-make 'third))) 'third)

(test-assert "lexp-member-false"
  (not (lexp-set-member test-set (lexp-make 'false))))

;;;;; LEXP Set resolution

(test-eqv "lexp-set-resolve-short"
  (set-id (lexp-set-resolve test-set (lexp-make 'test))) 'test)

(test-eqv "lexp-set-resolve-long"
  (set-id (lexp-set-resolve test-set (lexp-make 'test 'second))) 'second)

(test-assert "lexp-set-resolve-false"
  (match (lexp-set-resolve test-set (lexp-make 'wrong 'lexp))
    (($ <nothing> 'lexp-unknown) #t)
    (_ #f)))

;;;;; LEXP macro

(test-eqv "lexp-macro"
  (set-id (lexp-set-resolve test-set (lexp (test third)))) 'third)

;; LEXP utilities

(test-assert "set-lexp"
  (and (eqv? (lexp-base (set-lexp test-set)) 'test)
       (null? (lexp-rest (set-lexp test-set)))))

(test-assert "set-child-lexps"
  (match (set-child-lexps test-module)
    ((($ <lexp> 'root (four)) ($ <lexp> 'root (two))
      ($ <lexp> 'root (seven)))
     #t)
    (_ #f)))

(test-assert "set-child-lexps-with-lexp"
  (match (set-child-lexps test-module (lexp (test)))
    ((($ <lexp> 'test (root four)) ($ <lexp> 'test (root two))
      ($ <lexp> 'test (root seven)))
     #t)
    (_ #f)))

(test-assert "rootset-child-lexps"
  (null? (set-child-lexps ($mk-rootset))))

(test-assert "discipline-tree"
  ;; Each entry is a pair: (lexp . (list children) | #f)
  (match (discipline-tree test-module)
    (

     (($ <lexp> 'root ())
      (($ <lexp> 'root (four))
       (($ <lexp> 'root (four four-one)) . '()))
      (($ <lexp> 'root (two)) . '())
      (($ <lexp> 'root (seven))
       (($ <lexp> 'root (seven seven-one)) . '())
       (($ <lexp> 'root (seven seven-two)) . '())))

     #t)
    (_ #f)))

(test-assert "discipline-serialized-tree"
  (match (discipline-serialized-tree test-module)
    (
     ((root)
      ((root four) ((root four four-one)))
      ((root two))
      ((root seven) ((root seven seven-one)) ((root seven seven-two))))
     #t)
    (_ #f)))

(test-equal "discipline-tree->serialized-conversion"
  (discipline-serialized->tree
   (discipline-tree->serialized (discipline-tree test-module)))
  (discipline-tree test-module))

(test-assert "discipline-ancestry-tree"
  (match (discipline-ancestry-tree test-module test-ancestor)
    (
     ((#f . y4euxsglh3pe3fimxpt6dbsq7ihfz3ykqzvxt5pfazfcojqmvuqa)
      ((h4hgf3bshpvkwb5cjzqnqb6fmh5ztammgzv43zcnyusak4wkfgoq
        . yweuuld6bvrmynevqdurvrki2gcilr2xwrz76tz64u7owduu5lra)
       ((#f . "agfzfy5mgzqoe6ryntnbraobleacdw43nrgbzexock4koyzewx6a")))
      ((#f . "ktf43vwrqz2l6ny3el6sdhdex3l7wqsrtxdad2dzbw3z5wuzxdeq"))
      ((noz374bevnzqd55u5m2ednnd3fhdskiliwzl5g3hkwnfmdh5ryfq
        . t7fvvpxennlskntq3xsqzbh66nnf46f76dikckdylecwq7tblw6q)
       ((#f . "ettrdnlvtfa35dxaumzn37lpiibzlkviei4a2m4rnk5wvq3wupea"))
       ((#f . "w6gwg45jbp47trkrbcqum6lws3zq3qmtgdw3zjevvsypt7rmnepq"))))
     #t)
    (_ #f)))

(test-assert "discipline-ancestry-tree-unresolvable"
  (match (discipline-ancestry-tree test-module-false test-ancestor)
    (
     ((#f . y4euxsglh3pe3fimxpt6dbsq7ihfz3ykqzvxt5pfazfcojqmvuqa)
      (((? nothing?)
        . yweuuld6bvrmynevqdurvrki2gcilr2xwrz76tz64u7owduu5lra)
       ((#f . "agfzfy5mgzqoe6ryntnbraobleacdw43nrgbzexock4koyzewx6a")))
      ((#f . "ktf43vwrqz2l6ny3el6sdhdex3l7wqsrtxdad2dzbw3z5wuzxdeq"))
      (((? nothing?)
        . t7fvvpxennlskntq3xsqzbh66nnf46f76dikckdylecwq7tblw6q)
       ((#f . "ettrdnlvtfa35dxaumzn37lpiibzlkviei4a2m4rnk5wvq3wupea"))
       ((#f . "w6gwg45jbp47trkrbcqum6lws3zq3qmtgdw3zjevvsypt7rmnepq"))))
     #t)
    (_ #f)))

(test-end)

;;; lexp ends here
