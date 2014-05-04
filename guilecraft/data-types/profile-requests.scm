;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types profile-requests)
  #:use-module (rnrs records procedural)
  #:export (
            echoq
            echoq?
            echoq-token
            echoq-message
            echos
            echos?
            echos-token
            echos-lounge
            echos-library
            echos-message

            regq
	    regq?
	    regq-name
	    regq-prof-server
	    regq-mod-server

	    set!q
	    set!q?
	    set!q-token
	    set!q-field
	    set!q-value

	    set!s
	    set!s?
	    set!s-token
	    set!s-field
	    set!s-value

	    delq
	    delq?
	    delq-token

	    authq
	    authq?
	    authq-name
	    auths
	    auths?
	    auths-token
	    auths-mod-server
	    auths-prof-server

	    chauthq
	    chauthq?
	    chauthq-token
	    chauths
	    chauths?
	    chauths-token
	    chauths-hash
	    chauths-counter

	    evauthq
	    evauthq?
	    evauthq-token
	    evauthq-result))

;; Echo requests are a simple test request that returns a new token,
;; the profile-server, the module-server and the test-message supplied
;; to the requestor.
;;Request
(define echoq-rtd
  (make-record-type-descriptor 'echoq #f #f #f #f
			       '#((immutable token)
                                  (immutable message))))
(define echoq-rcd
  (make-record-constructor-descriptor echoq-rtd #f #f))
(define echoq (record-constructor echoq-rcd))
(define echoq? (record-predicate echoq-rtd))
(define echoq-token (record-accessor echoq-rtd 0))
(define echoq-message (record-accessor echoq-rtd 1))
;; Response
(define echos-rtd
  (make-record-type-descriptor 'echos #f #f #f #f
			       '#((immutable token)
				  (immutable prof-server)
				  (immutable mod-server)
                                  (immutable message))))
(define echos-rcd
  (make-record-constructor-descriptor echos-rtd #f #f))
(define echos (record-constructor echos-rcd))
(define echos? (record-predicate echos-rtd))
(define echos-token (record-accessor echos-rtd 0))
(define echos-lounge (record-accessor echos-rtd 1))
(define echos-library (record-accessor echos-rtd 2))
(define echos-message (record-accessor echos-rtd 3))

;; Reg requests provide a profile name and a module server (no
;; password for now) and are responded to with an auth-response.
(define regq-rtd
  (make-record-type-descriptor 'regq #f #f #f #f
			       '#((immutable name)
				  (immutable prof-server)
				  (immutable mod-server))))
(define regq-rcd
  (make-record-constructor-descriptor regq-rtd #f #f))
(define regq (record-constructor regq-rcd))
(define regq? (record-predicate regq-rtd))
(define regq-name (record-accessor regq-rtd 0))
(define regq-prof-server (record-accessor regq-rtd 1))
(define regq-mod-server (record-accessor regq-rtd 2))

;; Set! Requests provide a token, a symbol identifier of the field
;; indicated for modification (e.g. 'active-modules, 'name,
;; 'prof-server, etc.) and an appropriate new value for that field.
;; If no further action is required, return auths, else set!s.
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

;; Indicate to the client that further action is required.
(define set!s-rtd
  (make-record-type-descriptor 'set!s #f #f #f #f
			       '#((immutable token)
				  (immutable field)
				  (immutable value))))
(define set!s-rcd
  (make-record-constructor-descriptor set!s-rtd #f #f))
(define set!s (record-constructor set!s-rcd))
(define set!s? (record-predicate set!s-rtd))
(define set!s-token (record-accessor set!s-rtd 0))
(define set!s-field (record-accessor set!s-rtd 1))
(define set!s-value (record-accessor set!s-rtd 2))

;; Auth requests provide a profile name (no password for now) and are
;; responded to with a auth-response.
(define authq-rtd
  (make-record-type-descriptor 'authq #f #f #f #f
			       '#((immutable name))))
(define authq-rcd
  (make-record-constructor-descriptor authq-rtd #f #f))
(define authq (record-constructor authq-rcd))
(define authq? (record-predicate authq-rtd))
(define authq-name (record-accessor authq-rtd 0))

;; Auth responses provide a token to be used by the client for future
;; requests
(define auths-rtd
  (make-record-type-descriptor 'auths #f #f #f #f
			       '#((immutable token)
                                  (immutable prof-server)
				  (immutable mod-server))))
(define auths-rcd
  (make-record-constructor-descriptor auths-rtd #f #f))
(define auths (record-constructor auths-rcd))
(define auths? (record-predicate auths-rtd))
(define auths-token (record-accessor auths-rtd 0))
(define auths-prof-server (record-accessor auths-rtd 1))
(define auths-mod-server (record-accessor auths-rtd 2))

;; Chauthq requires a token and responds normally with chauths.
(define chauthq-rtd
  (make-record-type-descriptor 'chauthq #f #f #f #f
			       '#((immutable token))))
(define chauthq-rcd
  (make-record-constructor-descriptor chauthq-rtd #f #f))
(define chauthq (record-constructor chauthq-rcd))
(define chauthq? (record-predicate chauthq-rtd))
(define chauthq-token (record-accessor chauthq-rtd 0))

;; Chauths provide the blobhash and counter of the next challenge to
;; be asked, as well as the mod-server to be used by the client.
(define chauths-rtd
  (make-record-type-descriptor 'chauths #f #f #f #f
			       '#((immutable token)
				  (immutable hash)
				  (immutable counter))))
(define chauths-rcd
  (make-record-constructor-descriptor chauths-rtd #f #f))
(define chauths (record-constructor chauths-rcd))
(define chauths? (record-predicate chauths-rtd))
(define chauths-token (record-accessor chauths-rtd 0))
(define chauths-hash (record-accessor chauths-rtd 1))
(define chauths-counter (record-accessor chauths-rtd 2))

;; Evauth requests provide a token originally furnished in the most
;; recent auth request, to be used by the prof-server to identify the
;; scorecard to update in light of the result.
(define evauthq-rtd
  (make-record-type-descriptor 'evauthq #f #f #f #f
			       '#((immutable token)
				  (immutable result))))
(define evauthq-rcd
  (make-record-constructor-descriptor evauthq-rtd #f #f))
(define evauthq (record-constructor evauthq-rcd))
(define evauthq? (record-predicate evauthq-rtd))
(define evauthq-token (record-accessor evauthq-rtd 0))
(define evauthq-result (record-accessor evauthq-rtd 1))

;; Del Requests provide a token to identify the player profile to be
;; deleted.
(define delq-rtd
  (make-record-type-descriptor 'delq #f #f #f #f
			       '#((immutable token))))
(define delq-rcd
  (make-record-constructor-descriptor delq-rtd #f #f))
(define delq (record-constructor delq-rcd))
(define delq? (record-predicate delq-rtd))
(define delq-token (record-accessor delq-rtd 0))
