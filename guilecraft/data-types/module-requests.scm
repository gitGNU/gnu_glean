;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types module-requests)
  #:use-module (rnrs records procedural)
  #:export (challq
	    challq?
            challq-hash
            challq-counter

	    evalq
	    evalq?
	    evalq-hash
	    evalq-counter
	    evalq-answer

	    challs
	    challs?
	    challs-challenge

	    evals
	    evals?
	    evals-result
	    evals-solution

	    known-modq
	    known-modq?
	    known-modq-token

	    known-mods
	    known-mods?
	    known-mods-token
	    known-mods-list

	    hashmapq
	    hashmapq?
	    hashmapq-token
	    hashmapq-ids
	    hashmaps
	    hashmaps?
	    hashmaps-token
	    hashmaps-content

	    sethashesq
	    sethashesq?
	    sethashesq-token
	    sethashesq-set-ids

	    sethashess
	    sethashess?
	    sethashess-token
	    sethashess-set-ids))

;;; Game Requests
(define challq-rtd
  (make-record-type-descriptor 'challq #f #f #f #f
			       '#((immutable blobhash)
				  (immutable blobcounter))))
(define challq-rcd
  (make-record-constructor-descriptor challq-rtd #f #f))
(define challq (record-constructor challq-rcd))
(define challq? (record-predicate challq-rtd))
(define challq-hash (record-accessor challq-rtd 0))
(define challq-counter (record-accessor challq-rtd 1))

(define challs-rtd
  (make-record-type-descriptor 'challs #f #f #f #f
			       '#((immutable challenge))))
(define challs-rcd
  (make-record-constructor-descriptor challs-rtd #f #f))
(define challs (record-constructor challs-rcd))
(define challs? (record-predicate challs-rtd))
(define challs-challenge (record-accessor challs-rtd 0))

(define evalq-rtd
  (make-record-type-descriptor 'evalq #f #f #f #f
			       '#((immutable hash)
				  (immutable counter)
				  (immutable answer))))
(define evalq-rcd
  (make-record-constructor-descriptor evalq-rtd #f #f))
(define evalq (record-constructor evalq-rcd))
(define evalq? (record-predicate evalq-rtd))
(define evalq-hash (record-accessor evalq-rtd 0))
(define evalq-counter (record-accessor evalq-rtd 1))
(define evalq-answer (record-accessor evalq-rtd 2))

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

;; Request a list of known modules
(define known-modq-rtd
  (make-record-type-descriptor 'known-modq #f #f #f #f
			       '#((immutable token))))
(define known-modq-rcd
  (make-record-constructor-descriptor known-modq-rtd #f #f))
(define known-modq (record-constructor known-modq-rcd))
(define known-modq? (record-predicate known-modq-rtd))
(define known-modq-token (record-accessor known-modq-rtd 0))

;; Return a list of known modules
(define known-mods-rtd
  (make-record-type-descriptor 'known-mods #f #f #f #f
			       '#((immutable token)
				  (immutable list))))
(define known-mods-rcd
  (make-record-constructor-descriptor known-mods-rtd #f #f))
(define known-mods (record-constructor known-mods-rcd))
(define known-mods? (record-predicate known-mods-rtd))
(define known-mods-token (record-accessor known-mods-rtd 0))
(define known-mods-list (record-accessor known-mods-rtd 1))

;; hashmap requests ask the mod server for a nested list of sethashes
;; reflecting the set structure for the sets identified by the
;; set ids.
(define hashmapq-rtd
  (make-record-type-descriptor 'hashmapq #f #f #f #f
			       '#((immutable blobhashes))))
(define hashmapq-rcd
  (make-record-constructor-descriptor hashmapq-rtd #f #f))
(define hashmapq (record-constructor hashmapq-rcd))
(define hashmapq? (record-predicate hashmapq-rtd))
(define hashmapq-ids (record-accessor hashmapq-rtd 0))

;; hashmap responses provide a nested list of blobhashes, for the
;; requesting profile server.
(define hashmaps-rtd
  (make-record-type-descriptor 'hashmaps #f #f #f #f
			       '#((immutable content))))
(define hashmaps-rcd
  (make-record-constructor-descriptor hashmaps-rtd #f #f))
(define hashmaps (record-constructor hashmaps-rcd))
(define hashmaps? (record-predicate hashmaps-rtd))
(define hashmaps-content (record-accessor hashmaps-rtd 0))

;; Requires '(set-id …) pairs
(define sethashesq-rtd
  (make-record-type-descriptor 'sethashesq #f #f #f #f
			       '#((immutable set-ids))))
(define sethashesq-rcd
  (make-record-constructor-descriptor sethashesq-rtd #f #f))
(define sethashesq (record-constructor sethashesq-rcd))
(define sethashesq? (record-predicate sethashesq-rtd))
(define sethashesq-set-ids (record-accessor sethashesq-rtd 0))

;; Return '((set-id . sethash) …) pairs
(define sethashess-rtd
  (make-record-type-descriptor 'sethashess #f #f #f #f
			       '#((immutable set-ids))))
(define sethashess-rcd
  (make-record-constructor-descriptor sethashess-rtd #f #f))
(define sethashess (record-constructor sethashess-rcd))
(define sethashess? (record-predicate sethashess-rtd))
(define sethashess-set-ids (record-accessor sethashess-rtd 0))
