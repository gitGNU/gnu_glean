;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types requests)
  #:use-module (rnrs records procedural)
  #:export (request
	    request?
	    rq-content

	    response
	    response?
	    rs-content

	    alive-rq
	    alive-rq?
	    auth-rq
	    auth-rq?
	    auth-id
	    profs-rq
	    profs-rq?
	    auth-rs
	    auth-rs?
	    auth-rs-profile
	    profs-list
	    profs-rs
	    profs-rs?

	    chall-rq
	    chall-rq?
	    chall-rq-profile
	    eval-rq
	    eval-rq?
	    eval-rq-answer
	    eval-rq-profile
	    chall-rs
	    chall-rs?
	    chall-rs-challenge
	    chall-rs-profile
	    eval-rs-result
	    eval-rs-profile
	    eval-rs
	    eval-rs?

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

;; Query a list of known profiles from server. Returns profs-rs.
(define profs-rq-rtd
  (make-record-type-descriptor 'profs-rq #f #f #f #f '#()))
(define profs-rq-rcd
  (make-record-constructor-descriptor profs-rq-rtd #f #f))
(define profs-rq (record-constructor profs-rq-rcd))
(define profs-rq? (record-predicate profs-rq-rtd))

;; Provides a list of profiles as returned by (list-profiles) in the
;; list field.
(define profs-rs-rtd
  (make-record-type-descriptor 'profs-rs #f #f #f #f
			       '#((immutable list))))
(define profs-rs-rcd
  (make-record-constructor-descriptor profs-rs-rtd #f #f))
(define profs-rs (record-constructor profs-rs-rcd))
(define profs-rs? (record-predicate profs-rs-rtd))
(define profs-list (record-accessor profs-rs-rtd 0))

;; Auth requests provide a profile id (no password for now) and are
;; responded to with a auth-response.
(define auth-rq-rtd
  (make-record-type-descriptor 'auth-rq #f #f #f #f
			       '#((immutable id))))
(define auth-rq-rcd
  (make-record-constructor-descriptor auth-rq-rtd #f #f))
(define auth-rq (record-constructor auth-rq-rcd))
(define auth-rq? (record-predicate auth-rq-rtd))
(define auth-id (record-accessor auth-rq-rtd 0))

;; Returns a full profile in the profile field.
(define auth-rs-rtd
  (make-record-type-descriptor 'auth-rs #f #f #f #f
			       '#((immutable profile))))
(define auth-rs-rcd
  (make-record-constructor-descriptor auth-rs-rtd #f #f))
(define auth-rs (record-constructor auth-rs-rcd))
(define auth-rs? (record-predicate auth-rs-rtd))
(define auth-rs-profile (record-accessor auth-rs-rtd 0))

;; Requests the server quits cleanly. Provides an ack or neg response
;; in return as appropriate.
(define quit-rq-rtd
  (make-record-type-descriptor 'quit-rq #f #f #f #f '#()))
(define quit-rq-rcd
  (make-record-constructor-descriptor quit-rq-rtd #f #f))
(define quit-rq (record-constructor quit-rq-rcd))
(define quit-rq? (record-predicate quit-rq-rtd))

;;; Game Requests
(define chall-rq-rtd
  (make-record-type-descriptor 'chall-rq #f #f #f #f
			       '#((immutable profile))))
(define chall-rq-rcd
  (make-record-constructor-descriptor chall-rq-rtd #f #f))
(define chall-rq (record-constructor chall-rq-rcd))
(define chall-rq? (record-predicate chall-rq-rtd))
(define chall-rq-profile (record-accessor chall-rq-rtd 0))

(define chall-rs-rtd
  (make-record-type-descriptor 'chall-rs #f #f #f #f
			       '#((immutable profile)
				  (immutable challenge))))
(define chall-rs-rcd
  (make-record-constructor-descriptor chall-rs-rtd #f #f))
(define chall-rs (record-constructor chall-rs-rcd))
(define chall-rs? (record-predicate chall-rs-rtd))
(define chall-rs-profile (record-accessor chall-rs-rtd 0))
(define chall-rs-challenge (record-accessor chall-rs-rtd 1))

(define eval-rq-rtd
  (make-record-type-descriptor 'eval-rq #f #f #f #f
			       '#((immutable profile)
				  (immutable answer))))
(define eval-rq-rcd
  (make-record-constructor-descriptor eval-rq-rtd #f #f))
(define eval-rq (record-constructor eval-rq-rcd))
(define eval-rq? (record-predicate eval-rq-rtd))
(define eval-rq-profile (record-accessor eval-rq-rtd 0))
(define eval-rq-answer (record-accessor eval-rq-rtd 1))

(define eval-rs-rtd
  (make-record-type-descriptor 'eval-rs #f #f #f #f
			       '#((immutable profile)
				  (immutable result))))
(define eval-rs-rcd
  (make-record-constructor-descriptor eval-rs-rtd #f #f))
(define eval-rs (record-constructor eval-rs-rcd))
(define eval-rs? (record-predicate eval-rs-rtd))
(define eval-rs-profile (record-accessor eval-rs-rtd 0))
(define eval-rs-result (record-accessor eval-rs-rtd 1))

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
			       '#((immutable original))))
(define neg-rs-rcd
  (make-record-constructor-descriptor neg-rs-rtd #f #f))
(define neg-rs (record-constructor neg-rs-rcd))
(define neg-rs? (record-predicate neg-rs-rtd))
(define neg-orig (record-accessor neg-rs-rtd 0))

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
