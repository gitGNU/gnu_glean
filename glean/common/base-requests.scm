;;; glean --- Fast learning tool.         -*- coding: utf-8 -*-

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

;;; 
;; Define Requests
;;;
;; Requests are the formalised means by which communication between
;; the glean server and clients takes place.
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
;; acks, if it is alive.
(define aliveq-rtd
  (make-record-type-descriptor 'aliveq #f #f #f #f '#()))
(define aliveq-rcd
  (make-record-constructor-descriptor aliveq-rtd #f #f))
(define aliveq (record-constructor aliveq-rcd))
(define aliveq? (record-predicate aliveq-rtd))

;; Requests the server quits cleanly. Provides an ack or neg response
;; in return as appropriate.
(define quitq-rtd
  (make-record-type-descriptor 'quitq #f #f #f #f '#()))
(define quitq-rcd
  (make-record-constructor-descriptor quitq-rtd #f #f))
(define quitq (record-constructor quitq-rcd))
(define quitq? (record-predicate quitq-rtd))

;;; Generic Requests

;; Generic positive response. Should contain the original rq in the
;; original field.
(define acks-rtd
  (make-record-type-descriptor 'acks #f #f #f #f
			       '#((immutable original))))
(define acks-rcd
  (make-record-constructor-descriptor acks-rtd #f #f))
(define acks (record-constructor acks-rcd))
(define acks? (record-predicate acks-rtd))
(define ack-orig (record-accessor acks-rtd 0))

;; Generic negative response. Should contain the original rq in the
;; original field.
(define negs-rtd
  (make-record-type-descriptor 'negs #f #f #f #f
			       '#((immutable original)
				  (immutable message))))
(define negs-rcd
  (make-record-constructor-descriptor negs-rtd #f #f))
(define negs (record-constructor negs-rcd))
(define negs? (record-predicate negs-rtd))
(define neg-orig (record-accessor negs-rtd 0))
(define neg-msg (record-accessor negs-rtd 1))

;; Generic response in case of unknown data or request. Should contain
;; the original rq and data in the original field.
(define unks-rtd
  (make-record-type-descriptor 'unks #f #f #f #f
			       '#((immutable original))))
(define unks-rcd
  (make-record-constructor-descriptor unks-rtd #f #f))
(define unks (record-constructor unks-rcd))
(define unks? (record-predicate unks-rtd))
(define unk-orig (record-accessor unks-rtd 0))
