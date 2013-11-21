;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types base-requests)
  #:use-module (rnrs records procedural)
  #:export (request
	    request?
	    rq-content

	    response
	    response?
	    rs-content

	    alive-rq
	    alive-rq?

	    quit-rq
	    quit-rq?

	    ack-rs
	    ack-rs?
	    ack-orig
	    neg-rs
	    neg-rs?
	    neg-orig
	    unk-rs
	    unk-rs?
	    unk-orig))

;;; 
;; Define Requests
;;;
;; Requests are the formalised means by which communication between
;; the guilecraft server and clients takes place.
;;;

;; General wrapper around requests.This record will be expanded to
;; contain additional fields that prove useful.

(define request-rtd
  (make-record-type-descriptor 'request #f #f #f #f
			       '#((immutable content))))
(define request-rcd
  (make-record-constructor-descriptor request-rtd #f #f))
(define request (record-constructor request-rcd))
(define request? (record-predicate request-rtd))
(define rq-content (record-accessor request-rtd 0))

;; General wrapper around responses.This record will be expanded to
;; contain additional fields that prove useful.
(define response-rtd
  (make-record-type-descriptor 'response #f #f #f #f
			       '#((immutable content))))
(define response-rcd
  (make-record-constructor-descriptor response-rtd #f #f))
(define response (record-constructor response-rcd))
(define response? (record-predicate response-rtd))
(define rs-content (record-accessor response-rtd 0))

;; Check whether the server is alive. The server will respond with an
;; ack-rs, if it is alive.
(define alive-rq-rtd
  (make-record-type-descriptor 'alive-rq #f #f #f #f '#()))
(define alive-rq-rcd
  (make-record-constructor-descriptor alive-rq-rtd #f #f))
(define alive-rq (record-constructor alive-rq-rcd))
(define alive-rq? (record-predicate alive-rq-rtd))

;; Requests the server quits cleanly. Provides an ack or neg response
;; in return as appropriate.
(define quit-rq-rtd
  (make-record-type-descriptor 'quit-rq #f #f #f #f '#()))
(define quit-rq-rcd
  (make-record-constructor-descriptor quit-rq-rtd #f #f))
(define quit-rq (record-constructor quit-rq-rcd))
(define quit-rq? (record-predicate quit-rq-rtd))

;;; Generic Requests

;; Generic positive response. Should contain the original rq in the
;; original field.
(define ack-rs-rtd
  (make-record-type-descriptor 'ack-rs #f #f #f #f
			       '#((immutable original))))
(define ack-rs-rcd
  (make-record-constructor-descriptor ack-rs-rtd #f #f))
(define ack-rs (record-constructor ack-rs-rcd))
(define ack-rs? (record-predicate ack-rs-rtd))
(define ack-orig (record-accessor ack-rs-rtd 0))

;; Generic negative response. Should contain the original rq in the
;; original field.
(define neg-rs-rtd
  (make-record-type-descriptor 'neg-rs #f #f #f #f
			       '#((immutable original)
				  (immutable message))))
(define neg-rs-rcd
  (make-record-constructor-descriptor neg-rs-rtd #f #f))
(define neg-rs (record-constructor neg-rs-rcd))
(define neg-rs? (record-predicate neg-rs-rtd))
(define neg-orig (record-accessor neg-rs-rtd 0))
(define neg-msg (record-accessor neg-rs-rtd 1))

;; Generic response in case of unknown data or request. Should contain
;; the original rq and data in the original field.
(define unk-rs-rtd
  (make-record-type-descriptor 'unk-rs #f #f #f #f
			       '#((immutable original))))
(define unk-rs-rcd
  (make-record-constructor-descriptor unk-rs-rtd #f #f))
(define unk-rs (record-constructor unk-rs-rcd))
(define unk-rs? (record-predicate unk-rs-rtd))
(define unk-orig (record-accessor unk-rs-rtd 0))
