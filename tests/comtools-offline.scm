;; comtools-offline.scm --- basic communication tests    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 Jan 2014
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
;; Basic offline communication tests.
;;
;;; Code:

(define-module (tests comtools-offline)
  #:use-module (srfi srfi-64)           ; Provide test suite
  #:use-module (quickcheck quickcheck)
  #:use-module (tests quickcheck-defs)
  #:use-module (rnrs records inspection)
  #:use-module (glean common comtools))

(define record->list (@@ (glean common comtools) record->list))
(define list->record (@@ (glean common comtools) list->record))

(test-begin "comtools-offline")

(test-assert "record->list"
  (quickcheck (lambda (_) (list? (record->list _)))
              $record))
(test-assert "list->record"
  (quickcheck (lambda (_) (record? (list->record _)))
              $simple-tagged-list))
(test-assert "record->list*"
  (quickcheck (lambda (_) (list? (record->list* _)))
              $record))
(test-assert "list->record*"
  (quickcheck (lambda (_) (record? (list->record* _)))
              $tagged-list))

(test-end "comtools-offline")

;;; comtools-offline ends here
