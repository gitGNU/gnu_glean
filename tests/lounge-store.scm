;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

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

(define-module (tests lounge-store)
  #:use-module (srfi srfi-64)      ; Provide test suite
  #:use-module (guilecraft config)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft lounge-store)
  #:use-module (quickcheck quickcheck)
  #:use-module (tests quickcheck-defs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist))

(define ($mk-roothashtree)
  "Return a randomised hashmap built of only rootsets."
  ((@@ (guilecraft library-store) make-hashtree) ($mk-rootset)))
(define ($mk-hashtree)
  "Return a randomised hashmap built of only rootsets."
  ((@@ (guilecraft library-store) make-hashtree) ($mk-set)))
(define hashtree->blobs
  (@@ (guilecraft lounge-store) hashtree->blobs))
(define hashmap->blobs
  (@@ (guilecraft lounge-store) hashmap->blobs))

(test-begin "lounge-store")

(test-assert "hashtree->blobs"
  (quickcheck (lambda (_)
                (match (hashtree->blobs _)
                  (((? blob?) ...) #t)
                  (_ #f)))
              50 $mk-hashtree))
(test-assert "hashmap->blobs"
  (quickcheck (lambda (_) 
                (match (hashmap->blobs _)
                  (((? blob?) ...) #t)
                  (_ #f)))
              50 ($short-list $mk-hashtree)))

(test-end "lounge-store")
