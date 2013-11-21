;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types module-requests)
  #:use-module (rnrs records procedural)
  #:export (chall-rq
	    chall-rq?
	    challq-token
	    challq-auth-server

	    eval-rq
	    eval-rq?
	    evalq-token
	    evalq-auth-server
	    evalq-answer

	    chall-rs
	    chall-rs?
	    challs-challenge

	    eval-rs
	    eval-rs?
	    evals-result

	    hashmapq
	    hashmapq?
	    hashmapq-blobhashes
	    hashmaps
	    hashmaps?
	    hashmaps-content

	    hash-rq
	    hash-rq?
	    hashq-module-ids))

;;; Game Requests
(define chall-rq-rtd
  (make-record-type-descriptor 'chall-rq #f #f #f #f
			       '#((immutable token)
				  (immutable auth-server))))
(define chall-rq-rcd
  (make-record-constructor-descriptor chall-rq-rtd #f #f))
(define chall-rq (record-constructor chall-rq-rcd))
(define chall-rq? (record-predicate chall-rq-rtd))
(define challq-token (record-accessor chall-rq-rtd 0))
(define challq-auth-server (record-accessor chall-rq-rtd 1))

(define chall-rs-rtd
  (make-record-type-descriptor 'chall-rs #f #f #f #f
			       '#((immutable challenge))))
(define chall-rs-rcd
  (make-record-constructor-descriptor chall-rs-rtd #f #f))
(define chall-rs (record-constructor chall-rs-rcd))
(define chall-rs? (record-predicate chall-rs-rtd))
(define challs-challenge (record-accessor chall-rs-rtd 0))

(define eval-rq-rtd
  (make-record-type-descriptor 'eval-rq #f #f #f #f
			       '#((immutable token)
				  (immutable auth-server)
				  (immutable answer))))
(define eval-rq-rcd
  (make-record-constructor-descriptor eval-rq-rtd #f #f))
(define eval-rq (record-constructor eval-rq-rcd))
(define eval-rq? (record-predicate eval-rq-rtd))
(define evalq-token (record-accessor eval-rq-rtd 0))
(define evalq-auth-server (record-accessor eval-rq-rtd 1))
(define evalq-answer (record-accessor eval-rq-rtd 2))

(define eval-rs-rtd
  (make-record-type-descriptor 'eval-rs #f #f #f #f
			       '#((immutable result))))
(define eval-rs-rcd
  (make-record-constructor-descriptor eval-rs-rtd #f #f))
(define eval-rs (record-constructor eval-rs-rcd))
(define eval-rs? (record-predicate eval-rs-rtd))
(define evals-result (record-accessor eval-rs-rtd 0))

;; hashmap requests ask the mod server for a nested list of blobhashes
;; reflecting the set structure for the sets identified by the
;; blobhashes.
(define hashmapq-rtd
  (make-record-type-descriptor 'hashmapq #f #f #f #f
			       '#((immutable token)
				  (immutable crownset-ids)
				  (immutable blobhashes))))
(define hashmapq-rcd
  (make-record-constructor-descriptor hashmapq-rtd #f #f))
(define hashmapq (record-constructor hashmapq-rcd))
(define hashmapq? (record-predicate hashmapq-rtd))
(define hashmapq-token (record-accessor hashmapq-rtd 0))
(define hashmapq-crownset-ids (record-accessor hashmapq-rtd 1))
(define hashmapq-blobhashes (record-accessor hashmapq-rtd 2))

;; hashmap responses provide a nested list of blobhashes, for the
;; requesting profile server.
(define hashmaps-rtd
  (make-record-type-descriptor 'hashmaps #f #f #f #f
			       '#((immutable token)
				  (immutable content))))
(define hashmaps-rcd
  (make-record-constructor-descriptor hashmaps-rtd #f #f))
(define hashmaps (record-constructor hashmaps-rcd))
(define hashmaps? (record-predicate hashmaps-rtd))
(define hashmaps-token (record-accessor hashmaps-rtd 0))
(define hashmaps-content (record-accessor hashmaps-rtd 1))

;; Probably Obsolete
(define hash-rq-rtd
  (make-record-type-descriptor 'hash-rq #f #f #f #f
			       '#((immutable module-ids))))
(define hash-rq-rcd
  (make-record-constructor-descriptor hash-rq-rtd #f #f))
(define hash-rq (record-constructor hash-rq-rcd))
(define hash-rq? (record-predicate hash-rq-rtd))
(define hashq-module-ids (record-accessor hash-rq-rtd 0))
