;;; glean --- fast learning tool.         -*- coding: utf-8 -*-

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

(define-module (tests scorecards)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
  #:use-module (tests test-utils)
  #:use-module (quickcheck quickcheck)
  #:use-module (tests quickcheck-defs)

  #:use-module (glean utils)
  #:use-module (glean data-types scorecards))

(define number-of-child-blobs
  (@@ (glean data-types scorecards) number-of-child-blobs))
(define hashmap->blobs
  (@@ (glean lounge-store) hashmap->blobs))
(define make-hashtree
  (@@ (glean library-store) make-hashtree))
(define hashtree-map
  (@@ (glean lounge-store) hashtree-map))

(define (exp-growth num depth)
  (define (helper num depth total)
    (if (= depth 0)
        total
        (helper num (1- depth) (+ total (expt num depth)))))
  (helper num depth 0))

(define (mk-test-scorecard/selectors input hash-selector)
  (let ((hashmap ($mk-hashmap 1 input)))
    (cons (hash-selector hashmap)
          (add-blobs (hashmap->blobs hashmap)
                     (make-empty-scorecard)))))

(test-begin "scorecards")

(test-assert "basic blob creation"
  (quickcheck (lambda (_) (blob? _))
              $mk-rootblob))

(test-assert "basic scorecard creation"
  (quickcheck (lambda (_) (scorecard? _))
              50 $scorecard))

(define (number-of-child-tester input)
  "Return the result of number-of-child-tester for dynamically generated
INPUT."
  (let* ((crownhash/scard-pair (mk-test-scorecard/selectors input caaar))
         (crownhash (car crownhash/scard-pair))
         (scard (cdr crownhash/scard-pair)))
    (number-of-child-blobs (find-blob crownhash scard) scard)))

;; As a result of the memoization of number-of-child-blobs, occasional
;; incorrect counts may be produced if insufficient randomness exists in
;; the generated input (e.g. by using constant string values in the
;; quickcheck generators)

(test-assert "rootblob number of child blobs"
  (quickcheck (lambda (_)
                (= (number-of-child-tester _ )
                   (exp-growth 1 0)))
              (lambda () (lambda () ($mk-rootset 7)))))

(test-assert "blob number of child blobs"
  (quickcheck (lambda (_)
                (= (number-of-child-tester _)
                   (exp-growth 2 2)))
              50 (lambda () (lambda () ($mk-set 2 2)))))

(define modify-score
  (@@ (glean data-types scorecards) modify-score))

(define (mod-score-tester generator assess)
  "Check that scorecard updates match individual score-modify results for
GENERATOR derived dynamic input and the boolean ASSESS."
  (define (crownhash/roothash-selector hashmap)
    "Return the crownhash, and the hash of the first roothash."
    (define (recurse-until-root hashtree)
      (hashtree-map hashtree
                    (lambda (hash properties subtrees)
                      ;; ignore hash, properties)
                      (recurse-until-root (car subtrees)))
                    (lambda (hash properties)
                      ;; ignore properties)
                      hash)
                    (const #f)))
    (cons (caaar hashmap)
          (recurse-until-root (car hashmap))))

  (match (mk-test-scorecard/selectors generator crownhash/roothash-selector)
    (((crownhash . roothash) . scard)
     (let ((old-crownblob (find-blob crownhash scard)))
       (= (blob-score
           (find-blob crownhash
                      (update-scorecard scard roothash assess)))
          (modify-score (blob-score old-crownblob) assess
                        (number-of-child-blobs old-crownblob
                                               scard)))))))

(test-assert "update scorecard rootblob"
  (quickcheck mod-score-tester
              50 (lambda () (lambda () ($mk-rootset 7))) $boolean))

(test-assert "update scorecard blobs"
  (quickcheck mod-score-tester
              50 (lambda () (lambda () ($mk-set 3 2))) $boolean))

(test-end "scorecards")
