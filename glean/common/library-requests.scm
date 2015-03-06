;; library-requests.scm --- library server requests   -*- coding: utf-8 -*-
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
;; This module defines the vocabulary for requests specifically used when
;; communicating with a library server.
;;
;; Currently communications occurs through these distinct data-types,
;; serialized to plain lists by functionality defined in comtools, and then
;; transformed back into the respective data-types at the other end of the
;; socket.
;;
;; In order to reduce the depth of the comms tower before and after the use of
;; a socket we may switch to using simple lists which can be destructured with
;; (ice-9 match) instead of distinct data-types at a future point.
;;
;; The benefits of this would be that:
;; - we can carry out all input parsing at the point where data is read from
;;   the socket…
;; - we can extend the vocabulary in a simpler way by not having to define a
;;   new record type here…
;; - we remove the need for a hairy rnrs->list and list->rnrs record
;;   conversion for each communication sent…
;; - conversion to and from json/xml might be easier to implement.
;;
;;; Code:

(define-module (glean common library-requests)
  #:use-module (rnrs records procedural)
  #:export (challq
            <challq>
            challq?
            challq-lexp
            challq-dag-hash
            challq-shallow-hash
            challq-counter

            evalq
            <evalq>
            evalq?
            evalq-lexp
            evalq-dag-hash
            evalq-shallow-hash
            evalq-counter

            challs
            challs?
            challs-challenge

            evals
            evals?
            evals-result
            evals-solution

            knownq
            knownq-operator
            knownq-search
            knownq?
            knowns
            knowns?
            knowns-list

            detailq
            detailq?
            detailq-hash
            details
            details?
            details-list

            hashmapq
            hashmapq?
            hashmapq-token
            hashmapq-hashpairs
            hashmaps
            hashmaps?
            hashmaps-token
            hashmaps-content

            sethashesq
            sethashesq?
            sethashesq-token
            sethashesq-fullhashes

            sethashess
            sethashess?
            sethashess-token
            sethashess-hashpairs))


;;;;; Game Requests

;;;; Request Challenge
(define <challq> (make-record-type-descriptor '<challq> #f #f #f #f
                                              '#((immutable lexp)
                                                 (immutable dag-hash)
                                                 (immutable shallow-hash)
                                                 (immutable counter))))
(define challq-rcd (make-record-constructor-descriptor <challq> #f #f))
(define challq (record-constructor challq-rcd))
(define challq? (record-predicate <challq>))
(define challq-lexp (record-accessor <challq> 0))
(define challq-dag-hash (record-accessor <challq> 1))
(define challq-shallow-hash (record-accessor <challq> 2))
(define challq-counter (record-accessor <challq> 3))

;;;; Provide Challenge
(define challs-rtd
  (make-record-type-descriptor 'challs #f #f #f #f
                               '#((immutable challenge))))
(define challs-rcd
  (make-record-constructor-descriptor challs-rtd #f #f))
(define challs (record-constructor challs-rcd))
(define challs? (record-predicate challs-rtd))
(define challs-challenge (record-accessor challs-rtd 0))

;;;; Request Evaluation
(define <evalq> (make-record-type-descriptor '<evalq> #f #f #f #f
                                             '#((immutable lexp)
                                                (immutable dag-hash)
                                                (immutable shallow-hash)
                                                (immutable counter)
                                                (immutable answer))))
(define evalq-rcd (make-record-constructor-descriptor <evalq> #f #f))
(define evalq (record-constructor evalq-rcd))
(define evalq? (record-predicate <evalq>))
(define evalq-lexp (record-accessor <evalq> 0))
(define evalq-dag-hash (record-accessor <evalq> 1))
(define evalq-shallow-hash (record-accessor <evalq> 2))
(define evalq-counter (record-accessor <evalq> 3))
(define evalq-answer (record-accessor <evalq> 4))

;;;; Provide Evaluation
(define evals-rtd
  (make-record-type-descriptor 'evals #f #f #f #f
                               '#((immutable result)
                                  (immutable solution))))
(define evals-rcd
  (make-record-constructor-descriptor evals-rtd #f #f))
(define evals (record-constructor evals-rcd))
(define evals? (record-predicate evals-rtd))
(define evals-result (record-accessor evals-rtd 0))
(define evals-solution (record-accessor evals-rtd 1))


;;;; Request A List Of Known Modules
(define knownq-rtd
  (make-record-type-descriptor 'knownq #f #f #f #f
                               '#((immutable operator)
                                  (immutable search))))
(define knownq-rcd
  (make-record-constructor-descriptor knownq-rtd #f #f))
(define knownq (record-constructor knownq-rcd))
(define knownq? (record-predicate knownq-rtd))
(define knownq-operator (record-accessor knownq-rtd 0))
(define knownq-search (record-accessor knownq-rtd 1))

;;;; Provide A List Of Known Modules
(define knowns-rtd
  (make-record-type-descriptor 'knowns #f #f #f #f
                               '#((immutable list))))
(define knowns-rcd
  (make-record-constructor-descriptor knowns-rtd #f #f))
(define knowns (record-constructor knowns-rcd))
(define knowns? (record-predicate knowns-rtd))
(define knowns-list (record-accessor knowns-rtd 0))

;;;; Request Detail
;; Request detail for a specific module identified by its hash.
(define detailq-rtd
  (make-record-type-descriptor 'detailq #f #f #f #f
                               '#((immutable hash))))
(define detailq-rcd
  (make-record-constructor-descriptor detailq-rtd #f #f))
(define detailq (record-constructor detailq-rcd))
(define detailq? (record-predicate detailq-rtd))
(define detailq-hash (record-accessor detailq-rtd 0))

;; Provide Detail
;; Provide detail for a specific module.
(define details-rtd
  (make-record-type-descriptor 'details #f #f #f #f
                               '#((immutable list))))
(define details-rcd
  (make-record-constructor-descriptor details-rtd #f #f))
(define details (record-constructor details-rcd))
(define details? (record-predicate details-rtd))
(define details-list (record-accessor details-rtd 0))


;;;; Request Hashmap
;; Request hashmaps for sets identified by their IDs
(define hashmapq-rtd
  (make-record-type-descriptor 'hashmapq #f #f #f #f
                               '#((immutable hashpairs))))
(define hashmapq-rcd
  (make-record-constructor-descriptor hashmapq-rtd #f #f))
(define hashmapq (record-constructor hashmapq-rcd))
(define hashmapq? (record-predicate hashmapq-rtd))
(define hashmapq-hashpairs (record-accessor hashmapq-rtd 0))

;;;; Provide Hashmap
;; Provides hashmaps, a nested list of sethashes.
(define hashmaps-rtd
  (make-record-type-descriptor 'hashmaps #f #f #f #f
                               '#((immutable content))))
(define hashmaps-rcd
  (make-record-constructor-descriptor hashmaps-rtd #f #f))
(define hashmaps (record-constructor hashmaps-rcd))
(define hashmaps? (record-predicate hashmaps-rtd))
(define hashmaps-content (record-accessor hashmaps-rtd 0))

;;;; Ruequest Sethashes
;; Request the minhashes that go with the full sethashes provided.
(define sethashesq-rtd
  (make-record-type-descriptor 'sethashesq #f #f #f #f
                               '#((immutable fullhashes))))
(define sethashesq-rcd
  (make-record-constructor-descriptor sethashesq-rtd #f #f))
(define sethashesq (record-constructor sethashesq-rcd))
(define sethashesq? (record-predicate sethashesq-rtd))
(define sethashesq-fullhashes (record-accessor sethashesq-rtd 0))

;;;; Provide Sethashes
;; Provide the '(minghash . fullhash) sethash pairs requested above.
(define sethashess-rtd
  (make-record-type-descriptor 'sethashess #f #f #f #f
                               '#((immutable hashpairs))))
(define sethashess-rcd
  (make-record-constructor-descriptor sethashess-rtd #f #f))
(define sethashess (record-constructor sethashess-rcd))
(define sethashess? (record-predicate sethashess-rtd))
(define sethashess-hashpairs (record-accessor sethashess-rtd 0))

;;; library-requests.scm ends here
