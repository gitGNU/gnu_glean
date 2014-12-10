;; monadic-min.scm --- tests for monadic-min    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 13 November 2014
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
;; Unit tests for monadic-min.
;;
;; Source-file: glean/client/monadic-min.scm
;;
;;; Code:

(define-module (tests monadic-min)
  #:use-module (glean client monadic-min)
  #:use-module (glean common monads)
  #:use-module (glean common utils)
  #:use-module (srfi srfi-64)
  )


;;;; Tests

(test-begin "monadic-min")

(let ((proc (@@ (glean client monadic-min) client-monad-dict)))
  ;; test basic monad-dict functionality:
  ;; - test query
  ;; - shortening of stateful
  ;; - shortening of nothing
  ;; - long stateful
  ;; - long nothing
  (parameterize ((log-level 'exclaim))
    (test-equal "test"
      '(test-servers "Status:" success)
      (proc (stateful '(test) 'state) (log-level)))
    (test-equal "hash/fullhash-short"
      '(fetch-hashpairs "Hash/Fullhash:" ((("54321..." . "12345..."))))
      (proc (stateful '((("543210000" . "123450000"))) 'state) (log-level)))
    (test-equal "nothing-short"
      '(unknown "Nothing:" nothing-short)
      (proc (nothing 'nothing-short '(irrelevant)) (log-level))))
  (parameterize ((log-level 'all))
    (test-equal "hash/fullhash-long"
      '(fetch-hashpairs "Hash/Fullhash:" ((("543210000" . "123450000"))))
      (proc (stateful '((("543210000" . "123450000"))) 'state) (log-level)))
    (test-equal "nothing-long"
      '(unknown "Nothing:" (nothing-long . (irrelevant)))
      (proc (nothing 'nothing-long '(irrelevant)) (log-level)))))

(test-end)

;;; monadic-min ends here
