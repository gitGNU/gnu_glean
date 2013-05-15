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

(define-module (tests gmodules)
  #:use-module (srfi srfi-64)
  #:use-module (tests test-utils)

  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft gmodule-ops))

(test-begin "gmodule-tests")

(test-assert "Checking a gmodule"
	     (gmodule? test-gmodule))

(test-eq "Checking a gmodule ID"
	 'test
	 (gmodule-id test-gmodule))

(test-equal "Checking a gmodule name"
  "Test Gmodule"
  (gmodule-name test-gmodule))

(test-assert "Retrieving first gset"
  (let ([gmod-parts (gmodule-parts test-gmodule)])
    (gset_gset? (car-gsets gmod-parts))))

(test-assert "Recognising empty gmodule-parts"
  (let ([empty-gmod-parts (cdr-gsets (gmodule-parts test-gmodule))])
    (null-gsets? empty-gmod-parts)))

(test-end "gmodule-tests")
