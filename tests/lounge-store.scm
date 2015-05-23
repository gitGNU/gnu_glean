;; lounge-store.scm --- tests for lounge-store    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
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
;; Unit tests for lounge-store.
;;
;; Source-file: glean/lounge/lounge-store.scm
;;
;;; Code:

(define-module (tests lounge-store)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (glean config)
  #:use-module (glean common utils)
  #:use-module (glean common monads)
  #:use-module (glean library lexp)
  #:use-module (glean lounge scorecards)
  #:use-module (glean lounge lounge-store)
  #:use-module (quickcheck quickcheck)
  #:use-module (tests quickcheck-defs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  )


;;;; Tests

(define hashtree->blobs
  (@@ (glean lounge lounge-store) hashtree->blobs))
(define hashmap->blobs
  (@@ (glean lounge lounge-store) hashmap->blobs))

(test-begin "lounge-store")

;;;;; Tests for: lounge-monad-dict

(let ((proc (@@ (glean lounge lounge-store) lounge-monad-dict)))
  ;; test basic monad-dict functionality:
  ;; - test authenticate query
  ;; - shortening of stateful
  ;; - shortening of nothing
  ;; - long stateful
  ;; - long nothing
  (parameterize ((log-level 'exclaim))
    (let ((tk   75)
          (name "test")
          (id   'nothing-short))
      (test-equal "authenticate"
        `(authenticate "Token:" ,tk)
        (proc (stateful `(,tk) 'state) (log-level)))
      (test-equal "view-profile-short"
        `(view-profile "Profile:" ,name)
        (proc (stateful `((,name "lng" "lib" (act-mods))) 'state)
              (log-level)))
      (test-equal "nothing-short"
        `(unknown "Nothing:" ,id)
        (proc (stateful `(,(nothing id '(irrelevant))) 'state) (log-level)))))
  (parameterize ((log-level 'all))
    (let ((name     "test")
          (lng      "lng")
          (lib      "lib")
          (act-mods '(act-mods))
          (id      'nothing-long)
          (context '(irrelevant)))
      (test-equal "view-profile-long"
        `(view-profile "Profile:"
                       ,(string-join
                         `(,name ,lng ,lib ,(object->string act-mods)) ", "))
        (proc (stateful `((,name ,lng ,lib ,act-mods)) 'state)
              (log-level)))
      (test-equal "nothing-long"
        `(unknown "Nothing:" (,id . ,context))
        (proc (stateful `(,(nothing id context)) 'state) (log-level))))))

;;;;; Tests for: hashtree->blobs

(test-assert "hashtree->blobs"
  (quickcheck (lambda (_)
                (match (hashtree->blobs (lexp-make 'test) "dag-hash" _)
                  (((? blob?) ...) #t)
                  (_ #f)))
              10 $mk-hashtree))

;;;;; Tests for: hashmap->blobs

;; (test-assert "hashmap->blobs"
;;   (quickcheck (lambda (_)
;;                 (match (hashmap->blobs _)
;;                   (((? blob?) ...) #t)
;;                   (_ #f)))
;;               10 (lambda () `((lxp) "dag-hash" ,(($short-list $mk-hashtree))))))

;;;;; Tests for: modify-actives
(let ((modify-actives (@@ (glean lounge lounge-store) modify-actives))
      (new     (list (($pair (lambda () `(,($symbol))) $string))))
      (actives (($short-assoc (lambda () `(,($symbol))) $string #t))))
  (test-assert "modify-actives-add"
    (and
     ;; Prepend new active.
     (equal? (modify-actives actives new) (cons (car new) actives))
     ;; Ignore duplicate addition.
     (equal? (modify-actives actives `(,(car actives)))
             (cons (car actives)
                   (alist-delete (car (car actives)) actives)))))

  (test-assert "modify-actives-del"
    (and
     ;; delete an active
     (equal? (modify-actives actives `(,(car actives)) #t)
             (cdr actives))
     ;; ignore an unknown active
     (equal? (modify-actives actives new #t) actives))))

(test-end "lounge-store")

;;; lounge-store ends here
