;; base-requests.scm --- fundamental request structure   -*- coding: utf-8 -*-
;;
;; Copyright © 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
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
;; Module providing the fundamental container for requests (request,
;; response), and some basic request for communication.
;;
;; - aliveq simply tests whether a server is responsive;
;; - quitq sends a kill message to the server if it is supported;
;; - acks is an positive response;
;; - negs is a negative response;
;; - unks is a response indicating that the request was not understood.
;;
;;; Code:


(define-module (glean common base-requests)
  #:use-module (rnrs records procedural)
  #:export (request
            request?
            rq-content

            response
            response?
            rs-content

            aliveq
            aliveq?

            quitq
            quitq?

            acks
            acks?
            ack-orig
            negs
            negs?
            neg-orig
            neg-msg
            unks
            unks?
            unk-orig))

;;;; Requests & Responses
;;;
;;; Requests & Responses are the formalised means by which communication
;;; between the glean servers and clients takes place.

;;;; Request
;;; General wrapper around requests.  This record will be expanded to contain
;;; additional fields that prove useful, such as api version or a timestamp.
(define request-rtd (make-record-type-descriptor 'request #f #f #f #f
                                                 '#((immutable content))))
(define request-rcd (make-record-constructor-descriptor request-rtd #f #f))
(define request     (record-constructor request-rcd))
(define request?    (record-predicate request-rtd))
(define rq-content  (record-accessor request-rtd 0))

;;;; Response
;;; General wrapper around responses.  This record will be expanded to contain
;;; additional fields that prove useful, such as api version or a timestamp.
(define response-rtd (make-record-type-descriptor 'response #f #f #f #f
                                                  '#((immutable content))))
(define response-rcd (make-record-constructor-descriptor response-rtd #f #f))
(define response     (record-constructor response-rcd))
(define response?    (record-predicate response-rtd))
(define rs-content   (record-accessor response-rtd 0))


;;;; Generic Request Contents

;;;;; Aliveq
;;; Check whether the server is responsive.  The server will respond with an
;;; acks, if it is alive.
(define aliveq-rtd (make-record-type-descriptor 'aliveq #f #f #f #f '#()))
(define aliveq-rcd (make-record-constructor-descriptor aliveq-rtd #f #f))
(define aliveq     (record-constructor aliveq-rcd))
(define aliveq?    (record-predicate aliveq-rtd))

;;;;; Quitq
;; Requests the server quits cleanly.  Provides an ack or neg response in
;; return as appropriate.
(define quitq-rtd (make-record-type-descriptor 'quitq #f #f #f #f '#()))
(define quitq-rcd (make-record-constructor-descriptor quitq-rtd #f #f))
(define quitq     (record-constructor quitq-rcd))
(define quitq?    (record-predicate quitq-rtd))

;;;; Generic Response Contents

;;;;; Acks
;; Generic positive response.  Should contain the original rq in the original
;; field.
(define acks-rtd (make-record-type-descriptor 'acks #f #f #f #f
                                              '#((immutable original))))
(define acks-rcd (make-record-constructor-descriptor acks-rtd #f #f))
(define acks     (record-constructor acks-rcd))
(define acks?    (record-predicate acks-rtd))
(define ack-orig (record-accessor acks-rtd 0))

;;;;; Negs
;; Generic negative response.  Should contain the original rq in the original
;; field.
(define negs-rtd (make-record-type-descriptor 'negs #f #f #f #f
                                              '#((immutable original)
                                                 (immutable message))))
(define negs-rcd (make-record-constructor-descriptor negs-rtd #f #f))
(define negs     (record-constructor negs-rcd))
(define negs?    (record-predicate negs-rtd))
(define neg-orig (record-accessor negs-rtd 0))
(define neg-msg  (record-accessor negs-rtd 1))

;;;;; Unks
;; Generic response in case of unknown data or request.  Should contain the
;; original rq and data in the original field.
(define unks-rtd (make-record-type-descriptor 'unks #f #f #f #f
                                              '#((immutable original))))
(define unks-rcd (make-record-constructor-descriptor unks-rtd #f #f))
(define unks     (record-constructor unks-rcd))
(define unks?    (record-predicate unks-rtd))
(define unk-orig (record-accessor unks-rtd 0))

;;; base-requests.scm ends here
