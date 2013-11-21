;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types profile-requests)
  #:use-module (rnrs records procedural)
  #:export (reg-rq
	    reg-rq?
	    regq-name
	    regq-prof-server
	    regq-mod-server

	    modq
	    modq?
	    modq-token
	    modq-field
	    mods
	    mods?
	    mods-token
	    mods-value

	    set!q
	    set!q?
	    set!q-token
	    set!q-field
	    set!q-value

	    del-rq
	    del-rq?
	    delq-token

	    auth-rq
	    auth-rq?
	    authq-name
	    auth-rs
	    auth-rs?
	    auths-token
	    auths-prof-server

	    prep-challq
	    prep-challq?
	    prep-challq-token
	    prep-challs
	    prep-challs?
	    prep-challs-token
	    prep-challs-prof-server
	    prep-challs-mod-server

	    chauth-rq
	    chauth-rq?
	    chauthq-token
	    chauth-rs
	    chauth-rs?
	    chauths-token
	    chauths-hash
	    chauths-counter

	    evauth-rq
	    evauth-rq?
	    evauthq-token
	    evauthq-result))

;; Reg requests provide a profile name and a module server (no
;; password for now) and are responded to with an auth-response.
(define reg-rq-rtd
  (make-record-type-descriptor 'reg-rq #f #f #f #f
			       '#((immutable name)
				  (immutable prof-server)
				  (immutable mod-server))))
(define reg-rq-rcd
  (make-record-constructor-descriptor reg-rq-rtd #f #f))
(define reg-rq (record-constructor reg-rq-rcd))
(define reg-rq? (record-predicate reg-rq-rtd))
(define regq-name (record-accessor reg-rq-rtd 0))
(define regq-prof-server (record-accessor reg-rq-rtd 1))
(define regq-mod-server (record-accessor reg-rq-rtd 2))

;; Modq Request provide a symbol identifier of the field indicated
;; for modification (e.g. 'active-modules, 'name, 'prof-server, etc.)
(define modq-rtd
  (make-record-type-descriptor 'modq #f #f #f #f
			       '#((immutable token)
				  (immutable field))))
(define modq-rcd
  (make-record-constructor-descriptor modq-rtd #f #f))
(define modq (record-constructor modq-rcd))
(define modq? (record-predicate modq-rtd))
(define modq-token (record-accessor modq-rtd 0))
(define modq-field (record-accessor modq-rtd 1))

;; Mod Responses provide the value of the field identified in
;; modq back to the client.
(define mods-rtd
  (make-record-type-descriptor 'mods #f #f #f #f
			       '#((immutable token)
				  (immutable value))))
(define mods-rcd
  (make-record-constructor-descriptor mods-rtd #f #f))
(define mods (record-constructor mods-rcd))
(define mods? (record-predicate mods-rtd))
(define modq-token (record-accessor modq-rtd 0))
(define mods-value (record-accessor mods-rtd 1))

;; Set! Requests provide a token, a symbol identifier of the field
;; indicated for modification (e.g. 'active-modules, 'name,
;; 'prof-server, etc.) and an appropriate new value for that field.
(define set!q-rtd
  (make-record-type-descriptor 'set!q #f #f #f #f
			       '#((immutable token)
				  (immutable field)
				  (immutable value))))
(define set!q-rcd
  (make-record-constructor-descriptor set!q-rtd #f #f))
(define set!q (record-constructor set!q-rcd))
(define set!q? (record-predicate set!q-rtd))
(define set!q-token (record-accessor set!q-rtd 0))
(define set!q-field (record-accessor set!q-rtd 1))
(define set!q-value (record-accessor set!q-rtd 2))

;; Auth requests provide a profile name (no password for now) and are
;; responded to with a auth-response.
(define auth-rq-rtd
  (make-record-type-descriptor 'auth-rq #f #f #f #f
			       '#((immutable name))))
(define auth-rq-rcd
  (make-record-constructor-descriptor auth-rq-rtd #f #f))
(define auth-rq (record-constructor auth-rq-rcd))
(define auth-rq? (record-predicate auth-rq-rtd))
(define authq-name (record-accessor auth-rq-rtd 0))

;; Auth responses provide a token to be used by the client for future
;; requests
(define auth-rs-rtd
  (make-record-type-descriptor 'auth-rs #f #f #f #f
			       '#((immutable token)
				  (immutable prof-server))))
(define auth-rs-rcd
  (make-record-constructor-descriptor auth-rs-rtd #f #f))
(define auth-rs (record-constructor auth-rs-rcd))
(define auth-rs? (record-predicate auth-rs-rtd))
(define auths-token (record-accessor auth-rs-rtd 0))
(define auths-prof-server (record-accessor auth-rs-rtd 1))

;; prep-chall requests ask the prof server to prepare a new blobhash
;; for consumption by a mod server when it is ready to return a new
;; challenge.
;; prof server responds with a prep-chall response.
(define prep-challq-rtd
  (make-record-type-descriptor 'prep-challq #f #f #f #f
			       '#((immutable token))))
(define prep-challq-rcd
  (make-record-constructor-descriptor prep-challq-rtd #f #f))
(define prep-challq (record-constructor prep-challq-rcd))
(define prep-challq? (record-predicate prep-challq-rtd))
(define prep-challq-token (record-accessor prep-challq-rtd 0))

;; prep-chall responses provide a new token, the issuing profile
;; server address and the profile's associated mod server address.
(define prep-challs-rtd
  (make-record-type-descriptor 'prep-challs #f #f #f #f
			       '#((immutable token)
				  (immutable prof-server)
				  (immutable mod-server))))
(define prep-challs-rcd
  (make-record-constructor-descriptor prep-challs-rtd #f #f))
(define prep-challs (record-constructor prep-challs-rcd))
(define prep-challs? (record-predicate prep-challs-rtd))
(define prep-challs-token (record-accessor prep-challs-rtd 0))
(define prep-challs-prof-server (record-accessor prep-challs-rtd 1))
(define prep-challs-mod-server (record-accessor prep-challs-rtd 2))

;; Chauth requests provide the token originally provided by auth-rs to
;; the client, supplied by a mod-server in expectation of a chauth
;; response: the information required to generate the next challenge.
(define chauth-rq-rtd
  (make-record-type-descriptor 'chauth-rq #f #f #f #f
			       '#((immutable token))))
(define chauth-rq-rcd
  (make-record-constructor-descriptor chauth-rq-rtd #f #f))
(define chauth-rq (record-constructor chauth-rq-rcd))
(define chauth-rq? (record-predicate chauth-rq-rtd))
(define chauthq-token (record-accessor chauth-rq-rtd 0))

;; Chauth responses provide a token, a hash and a counter. The token
;; is to be returned to the client. The hash and the counter let the
;; mod-server identify the next challenge to pose.
(define chauth-rs-rtd
  (make-record-type-descriptor 'chauth-rs #f #f #f #f
			       '#((immutable token)
				  (immutable hash)
				  (immutable counter))))
(define chauth-rs-rcd
  (make-record-constructor-descriptor chauth-rs-rtd #f #f))
(define chauth-rs (record-constructor chauth-rs-rcd))
(define chauth-rs? (record-predicate chauth-rs-rtd))
(define chauths-token (record-accessor chauth-rs-rtd 0))
(define chauths-hash (record-accessor chauth-rs-rtd 1))
(define chauths-counter (record-accessor chauth-rs-rtd 2))

;; Evauth requests provide a token originally furnished in the most
;; recent auth request, to be used by the prof-server to identify the
;; scorecard to update in light of the result.
(define evauth-rq-rtd
  (make-record-type-descriptor 'evauth-rq #f #f #f #f
			       '#((immutable token)
				  (immutable result))))
(define evauth-rq-rcd
  (make-record-constructor-descriptor evauth-rq-rtd #f #f))
(define evauth-rq (record-constructor evauth-rq-rcd))
(define evauth-rq? (record-predicate evauth-rq-rtd))
(define evauthq-token (record-accessor evauth-rq-rtd 0))
(define evauthq-result (record-accessor evauth-rq-rtd 1))

;; Del Requests provide a token to identify the player profile to be
;; deleted.
(define del-rq-rtd
  (make-record-type-descriptor 'del-rq #f #f #f #f
			       '#((immutable token))))
(define del-rq-rcd
  (make-record-constructor-descriptor del-rq-rtd #f #f))
(define del-rq (record-constructor del-rq-rcd))
(define del-rq? (record-predicate del-rq-rtd))
(define delq-token (record-accessor del-rq-rtd 0))
