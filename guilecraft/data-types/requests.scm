;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types requests)
  #:use-module (srfi srfi-9)
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
(define-record-type <request>
  (request content)
  request?
  (content rq-content))

;; General wrapper around responses.This record will be expanded to
;; contain additional fields that prove useful.
(define-record-type <response>
  (response content)
  response?
  (content rs-content))

;; Check whether the server is alive. The server will respond with an
;; ack-rs, if it is alive.
(define-record-type <alive-rq>
  (alive-rq)
  alive-rq?)

;; Query a list of known profiles from server. Returns profs-rs.
(define-record-type <profs-rq>
  (profs-rq)
  profs-rq?)

;; Provides a list of profiles as returned by (list-profiles) in the
;; list field.
(define-record-type <profs-rs>
  (profs-rs list)
  profs-rs?
  (list profs-list))

;; Auth requests provide a profile id (no password for now) and are
;; responded to with a auth-response.
(define-record-type <auth-rq>
  (auth-rq id)
  auth-rq?
  (id auth-id))

;; Returns a full profile in the profile field.
(define-record-type <auth-rs>
  (auth-rs profile)
  auth-rs?
  (profile auth-rs-profile))

;; Requests the server quits cleanly. Provides an ack or neg response
;; in return as appropriate.
(define-record-type <quit-rq>
  (quit-rq)
  quit-rq?)

;;; Game Requests

(define-record-type <chall-rq>
  (chall-rq profile)
  chall-rq?
  (profile chall-rq-profile))

(define-record-type <chall-rs>
  (chall-rs profile challenge)
  chall-rs?
  (profile chall-rs-profile)
  (challenge chall-rs-challenge))

(define-record-type <eval-rq>
  (eval-rq profile answer)
  eval-rq?
  (profile eval-rq-profile)
  (answer eval-rq-answer))

(define-record-type <eval-rs>
  (eval-rs profile result)
  eval-rs?
  (profile eval-rs-profile)
  (result eval-rs-result))

;;; Generic Requests

;; Generic positive response. Should contain the original rq in the
;; original field.
(define-record-type <ack-rs>
  (ack-rs original)
  ack-rs?
  (original ack-orig))

;; Generic negative response. Should contain the original rq in the
;; original field.
(define-record-type <neg-rs>
  (neg-rs original)
  neg-rs?
  (original neg-orig))

;; Generic response in case of unknown data or request. Should contain
;; the original rq and data in the original field.
(define-record-type <unk-rs>
  (unk-rs original)
  unk-rs?
  (original unk-orig))
