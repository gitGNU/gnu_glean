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

;;;;; Shallow-hash
;;;
;;; We want shallow hash to be different only if #:version, lxp or the lxps of
;;; a given set's children change.  Conversely, any other field can change to
;;; its heart's content — shallow-hash should still be string=?.

(test-assert "shallow-hash-equal"
  (let ((set ($mk-set)))
    (and
     (string=? (shallow-hash (set-lexp set) set)
               (shallow-hash (set-lexp set) set))
     (string=? (shallow-hash (set-lexp set) set)
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
  (let ((set ($mk-set 2 2))
        (ne? (negate string=?)))
    (and
     (ne? (shallow-hash (set-lexp set) set) ; lexp diff
          (shallow-hash (lexp (set target)) set))
     (ne? (shallow-hash (set-lexp set) set) ; version diff
          (shallow-hash (set-lexp set)
                        (set-set-version set
                                         (string-reverse (set-version set)))))
     (ne? (shallow-hash (set-lexp set) set) ; child-lexp diff
          (shallow-hash (set-lexp set)
                        (set-set-contents set (cons ($mk-rootset)
                                                    (set-contents set))))))))

;;;;; Deep-hash
;;;
;;; We want deep hash only to be string=? to exactly a set that contains
;;; equal? values in each field.

;;; Are we the same if we deep-hash the same disc twice?
(test-assert "deep-hash-equal"
  (let ((disc ($mk-discipline 2 2)))
    (string=? (deep-hash disc) (deep-hash disc))))

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
  (let ((disc ($mk-discipline 2 2))
        (ne?  (negate string=?)))
    (fold (lambda (procs result)
            (if result
                (match procs
                  ((setter accessor transformer)
                   (ne? (deep-hash disc)
                        (deep-hash (setter disc (transformer
                                                 (accessor disc)))))))
                result))
          #t proc-list)))

;;; Are we different if we have the same disc, but change fields in the child?
(test-assert "deep-hash-not-equal-child"
  (let ((disc ($mk-discipline 2 2))
        (ne?  (negate string=?)))
    (fold (lambda (procs result)
            (if result
                (match procs
                  ((setter accessor transformer)
                   (ne? (deep-hash disc)
                        (deep-hash
                         (set-set-contents
                          disc
                          `(,(car (set-contents disc))
                            ,(setter (cadr (set-contents disc))
                                     (transformer
                                      (accessor
                                       (cadr (set-contents disc)))))))))))
                result))
          #t proc-list)))

;;;;; Dag-hash
;;;
;;; We want Dag-hash to equal as long as no new sets are added or removed in
;;; the discipline  or its children.
;;;
;;; Conversely, the hash should be different as soon as a set is added,
;;; removed, or moved to discipline or any of its children.

;;; Are we still the same if we change some fields?
(test-assert "dag-hash-equal"
  (let ((disc ($mk-discipline 2 2)))
    (and (string=? (dag-hash disc) (dag-hash disc))
         (string=? (dag-hash disc)
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
    (and (string=? (dag-hash disc) (dag-hash disc))
         (string=?
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
  (let* ((disc ($mk-discipline 2 2))
         (ne? (negate string=?)))
    (match (set-contents disc)
      ((1st 2nd)
       (fold (lambda (new-disc result)
               (if result (ne? (dag-hash disc) (dag-hash new-disc)) result))
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

(test-end "set-tools")

;;; set-tools ends here
