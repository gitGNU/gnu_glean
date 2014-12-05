;; library-store.scm --- library → filesystem interface   -*- coding: utf-8 -*-
;;
;; Copyright (C) 2012, 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
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
;; Provide functions to interface between the Glean Library and
;; the underlying filesystem.
;;
;; Functionality covers two areas:
;; 1) Manage loading of modules from filesystem into Glean
;; 2) Provide functionality to uniformly retrieve any data from
;;    Glean sets.
;;
;; In the medium term I want to turn the filesystem interface proper into an
;; 'extensible component', allowing arbitrary methods of storing disciplines
;; in persistent state (direct file-system reads, rdb interfaces, etc.).
;;
;; This would probably mean the re-writing of this interface to load
;; 'components' and use a standardized interface to interact with discipline
;; data (fetch arbitrary parts of those disciplines).  The current direct
;; filesystem interface would be moved to become the 'reference
;; implementation' of the above standard interface.
;;
;;; Tests:
;; (tests library-store)
;;
;;; Code:

(define-module (glean library library-store)
  #:use-module (glean common base32)
  #:use-module (glean common hash)
  #:use-module (glean common monads)
  #:use-module (glean common utils)
  #:use-module (glean library core-templates)
  #:use-module (glean library sets)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (
            compile-library
            catalogue-hash

            fetch-set
            set-fullhash
            set-hashpairs
            known-crownsets
            search-sets
            set-details
            hashtree?
            make-hashtree
            ))


;;;;; The Library
;;; A library is a database of two tables:
;;; 1) All known modules are indexed by their fullhashes.
;;; 2) A secondary reference from minhash to fullhash.  The latter can contain
;;;    one to many mappings and can be used to ensure continuity even after
;;;    module upgrades.
;;;
;;; A fullhash is a hash of a module's meta-data as well as its contents.
;;;
;;; A minhash is a hash of a module's author and id data.  Normally modules are
;;; identified using fullhashes only, but these will change when any changes to
;;; the module are made (e.g. version bumps).
;;;
;;; To allow mapping of hashes supplied by the lounge server from before the
;;; version bump, the minhash will be used instead of the fullhash.

(define-record-type <library>
  (library catalogue reference)
  library?
  (catalogue library-cat)
  (reference library-ref))

(define (empty-library)
  "Return an empty library."
  (library vlist-null vlist-null))

(define (empty-library? obj)
  "Return #t if OBJ is an empty library."
  (and (library? obj)
       (vlist-null? (library-cat obj))
       (vlist-null? (library-ref obj))))

(define (pretty-print-library lib)
  "Print the library LIB to stdout."
  (define (comp-string vlist . set?)
    (vlist-map (lambda (cat-entry)
                 (if (null? set?)
                     (string-format "~a: ~a"
                                    (car cat-entry)
                                    (cdr cat-entry))
                     (string-format "~a: ~a (~a)"
                                    (car cat-entry)
                                    (cdr cat-entry)
                                    (set-id (cdr cat-entry)))))
               vlist))
  (format #t "~a\n*Catalogue*:\n~a\n*Catalogue End*
*Reference*:\n~a\n*Reference End*\n"
          lib
          (string-join (vlist->list (comp-string (library-cat lib) #t))
                       "\n")
          (string-join (vlist->list (comp-string (library-ref lib)))
                       "\n")))


;;;;; Store API
;;;
;;; Any compiled-library dependent procedure (virtually all in the library)
;;; require first a call to the "compile-library" procedure. This is a lazy
;;; procedure that, given a catalogue hash, returns either a memoized library or
;;; compiles the catalogue a-fresh into a new library when the value is required.
;;;
;;; The idea here is that we could hot-swap the library, if the underlying
;;; filesystem structure changes (e.g. new files are added, existing
;;; definitions change).  We don't want to reload the library with every
;;; access to loaded disciplines though, so we cache the loaded library.
;;;
;;; The cache is implemented through the memoized procedure
;;; 'compile-library'.  It expects a catalogue-hash-pair, which is generated by
;;; 'catalogue-hash'. Catalogue-hash will return the sha-256 hash of the status of
;;; 'catalogue-dir', thus returning an identical value if 'catalogue-dir' has not
;;; changed.
;;;
;;; The mechanism described above seems to work — though it's efficacy in
;;; terms of comparative performance is currently unknown.
;;;
;;; The above is an example of the kind of logic that should be stripped out
;;; of Glean proper and into 'components' — in this case the
;;; library-filesystem component.

(define (catalogue-hash catalogue-dir)
  "Return a hash, representing the state of CATALOGUE-DIR."
  (if (file-exists? catalogue-dir)
      (cons catalogue-dir
            (call-with-values open-sha256-port
              (lambda (port reader)
                (write (file-system-tree catalogue-dir (const #t) stat) port)
                (close-port port)
                (reader))))
      #f))


;;; The current implementation of compile-library will return modules for
;;; core-dir and user-dir, but will only recompile the lounge on the basis of
;;; changes to user-dir (see catalogue-hash implementation and library-modules
;;; implementation as well as this one, compile-library).
;;;
;;; i.e., compile library assumes the core-dir is a read-only catalogue.
(define compile-library
  (let ((cached-hash    (make-bytevector 0))
        (cached-library (empty-library)))
    (lambda (catalogue-hash-pair)
      "Return library (a vhash) corresponding to the file-system state
represented by CATALOGUE-HASH-PAIR.  If the hash in CATALOGUE-HASH-PAIR was passed
to us before, return the previously computed library.  Else, re-compute."
      (match catalogue-hash-pair
        ((catalogue-dir . catalogue-hash)
         (define (compile)
           "Return the library vhash from the sets in LIBRARY-MODULES."
           (fold (lambda (module library)
                   (fold library-cons
                         library                     ; library thus far
                         (sets-from-module module))) ; these sets
                 (empty-library)                     ; new library
                 ;; If cached-library is not empty then we must force a reload
                 ;; of modules to be loaded as glean will be using cached
                 ;; modules otherwise.
                 ;; (This will be executed if the library catalogue has changed
                 ;; since the last time it was checked.)
                 (if (empty-library? cached-library)
                     (library-modules catalogue-dir) ; plain load
                     (filter-map (lambda (module)  ; force reload
                                   (false-if-exception (reload-module module)))
                                 (library-modules catalogue-dir)))))

         ;; If CATALOGUE-HASH has not changed, return cached library.
         (if (bytevector=? catalogue-hash cached-hash)
             cached-library
             (begin (set! cached-hash    catalogue-hash)
                    (set! cached-library (compile))
                    cached-library)))
        (#f cached-library)))))


;;;;; Library Convenience

(define (library-cons set lib)
  "Return a new library consisting of LIBRARY augmented by SET."
  (let ((fullhash (set-fullhash set))
        (minhash  (crownset-minhash  set))
        (cat      (library-cat  lib))
        (ref      (library-ref  lib)))
    (library (fold (lambda (hash-set-pair cat-so-far)
                     (if (lib-assoc (car hash-set-pair) cat-so-far)
                         cat-so-far
                         (lib-cons (car hash-set-pair)
                                   (cdr hash-set-pair)
                                   cat-so-far)))
                   cat
                   (crownset-hash-index set))
      (if (lib-assoc minhash ref) ; if minhash exists, append
          (let ((kv-pair (lib-assoc minhash ref)))
            (lib-cons minhash (cons fullhash (cdr kv-pair))
                      (lib-delete minhash ref)))
          (lib-cons minhash (list fullhash) ref)))))

(define (lib-cat library-pair)
  "Return the catalogue of sets derived from LIBRARY-PAIR."
  (library-cat (compile-library library-pair)))

(define (lib-ref library-pair)
  "Return the min-hash->full-hash catalogue derived from
LIBRARY-PAIR."
  (library-ref (compile-library library-pair)))

(define (lib-cons hash value component)
  "Return a new library component based on COMPONENT where KEY is ASSOCIATED
with VALUE."
  (vhash-cons hash value component))

(define (lib-assoc hash component)
  "Return the first hash/value pair from the library component COMPONENT whose
hash is equal to HASH."
  (vhash-assoc hash component))

(define (lib-delete hash component)
  "Remove all associations from the library component COMPONENT with HASH."
  (vhash-delete hash component))

(define (lib-fold proc init component)
  "Fold over the hash/value elements of the library component COMPONENT from
left to right, with each call to PROC having the form ‘(PROC hash value
result)’, where RESULT is the result of the previous call to PROC and INIT the
value of RESULT for the first call to PROC."
  (vhash-fold proc init component))


;;;;; Porcelain Library Procedures
;;; These are procedures most likely to be used outside of this module.

(define (fetch-set hash library-pair)
  "Return the set identified by HASH from the library derived from
LIBRARY-PAIR."
  (let ((set-pair? (lib-assoc hash (lib-cat library-pair))))
    (if set-pair? (cdr set-pair?) #f)))

(define (known-crownsets library-pair config-ignore-keywords)
  "Return a list containing summary information on all known crownsets
in the library derived from LIBRARY-PAIR, ignoring those that contain
keywords blacklisted in CONFIG-IGNORE-KEYWORDS."
  (fold (lambda (hash sets)
          (let ((set (fetch-set hash library-pair)))
            (if (null? (lset-intersection string=?
                                          config-ignore-keywords
                                          (set-keywords set)))
                (cons (set-summary set hash) sets)
                sets)))
        '()
        (known-crownset-hashes library-pair)))

(define (search-sets operator search library-pair)
  "Return a list containing summary information for all sets matching
the criteria specified by OPERATOR and SEARCH in LIBRARY-PAIR."
  (define (search-by-hash hashes)
    "Return the set summary of any sets identified by one of the
hashes in HASHES."
    (map (lambda (hash/set-pair)
           (set-summary (car hash/set-pair)   ; set
                        (cdr hash/set-pair))) ; fullhash
         (filter-map (lambda (hash)
                       (let ((set (fetch-set hash library-pair)))
                         (if set
                             (cons set hash)
                             #f)))
                     hashes)))
  (define (search-by-name strings)
    "Return the set summary of any sets matching a string in STRINGS
in their name."
    '())                                  ; not implemented yet.
  (cond ((eqv? operator 'match)           ; match operation
         (cond ((eqv? (car search) 'hash) ; hash search
                (search-by-hash (cdr search)))
               ((eqv? (car search) 'name) ; name search
                (search-by-name (cdr search)))
               (else
                '())))                  ; no other searches yet
        (else                           ; no other operation yet
         '())))

(define* (set-summary set #:optional (hash #f))
  "Return a list containing summary information on SET."
  (list (if hash hash (set-fullhash set))
        (set-id set)
        (set-name set)
        (set-version set)
        (set-keywords set)
        (set-synopsis set)
        (set-logo     set)))

(define (set-details hash library-pair)
  "Return a list containing detailed information on SET, or #f if it
cannot be found in LIBRARY-PAIR."
  (let ((set (fetch-set hash library-pair)))
    (if set
        (list hash
              (set-id set)
              (set-name set)
              (set-version set)
              (set-keywords set)
              (set-synopsis set)
              (set-description set)
              (set-creator set)
              (set-attribution set)
              (set-resources set)
              (set-properties set)
              (if (rootset? set)
                  '()
                  (map set-summary (set-contents set)))
              (set-logo set))
        #f)))

(define (known-crownset-hashes library-pair)
  "Return a list of all known crownset hashes in lib-ref derived
from LIBRARY-PAIR."
  (flatten (vlist->list (vlist-map cdr
                                   (lib-ref library-pair)))))

(define (set-hashpairs fullhashes library-pair)
  "Return a list of (minhash . fullhash) for each hash in FULLHASH, if
it is known in LIBRARY-PAIR."
  (map (lambda (fullhash)
         (let ((minhash (and=> (fetch-set fullhash library-pair)
                               crownset-minhash)))
           (if minhash
               (cons minhash fullhash)    ; valid hashpair
               (cons #f      fullhash)))) ; invalid fullhash
       fullhashes))


;;;;; Helpers

(define (string-format msg . args)
  "Return a string composed of MSG and ARGS, by constructing it using format."
  (with-output-to-string
    (lambda () (apply format #t msg args))))

(define (test-file filename)
  "Return #t if FILENAME identifies a regular file that we have access to, #f
otherwise."
  (catch 'system-error
    (lambda () (eqv? (stat:type (stat filename)) 'regular))
    (lambda (key . args) #f)))

(define (sets-from-module module)
  "Return a list containing each exported set in MODULE.  Ignore all exported
bindings which are not sets.

Should module not export any bindings, raise an error."
  (or (and=> (module-public-interface module)
             (lambda (mod-interface)
               (filter (lambda (set)
                         (if (plain-module? set)
                             set
                             (begin
                               (caution "~a: not a module, eliminating.~%"
                                        (cond ((set? set) (set-name set))
                                              ((nothing? set) 'nothing)
                                              (else set)))
                               #f)))
                       (module-map
                        (lambda (name value)
                          (inform "Loading ~a..." name)
                          (let ((maybe (resolve-set (variable-ref value))))
                            (if (set? maybe)
                                (inform "[success]~%")
                                (caution "[failure]~%"))
                            maybe))
                        mod-interface))))
      (error "SETS-FROM-MODULE -- module did not resolve:" module)))

(define (library-modules catalogue-dir)
  "Return the list of Guile modules, derived from the files in CATALOGUE-DIR,
that provide content for the library. These modules are then parsed using
`sets-from-module' to retrieve the Glean modules contained therein."
  (define* (data-files)
    "Return the list of files that implement glean modules."
    (define prefix-len (string-length catalogue-dir))
    (file-system-fold (const #t)                 ; enter?
                      (lambda (path stat result) ; leaf
                        (if (string=? (basename path) "discipline.scm")
                            (cons (substring path prefix-len) result)
                            result))
                      (lambda (path stat result) ; down
                        result)
                      (lambda (path stat result) ; up
                        result)
                      (const #f)        ; skip
                      (lambda (path stat errno result)
                        (error (_ "cannot access `~a': ~a~%")
                               path (strerror errno))
                        result)
                      '()
                      catalogue-dir
                      stat))
  (define (data-files->modules path)
    (define not-slash (char-set-complement (char-set #\/)))
    (let ((name (map string->symbol
                     (string-tokenize (string-drop-right path 4)
                                      not-slash))))
      (false-if-exception (resolve-module name))))

  (when (not (member catalogue-dir %load-path))
    (add-to-load-path catalogue-dir))
  (filter-map data-files->modules (data-files)))


;;;;; Procedures for inclusion in Set
;;;
;;; These procedures should really be located in (glean library sets), as they
;;; pertain to sets in general rather than their storage in the library.
;;;
;;; A) I'm not sure whether these should actually be pushed down to the Set
;;;    stack.  They feel library-store-like for me.
;;; B) This module needs to be split consistently into a component node, and
;;;    it's interface expectations (i.e., the reference implementation for the
;;;    component.)

;;;; Sethash Revisions: The Library Store, Upgrades & Set Identities
;;;
;;; Our new approach to set identities will be fully based on the set's
;;; hash.  This hash is contained in the set's hash field and consists of all
;;; other fields hashed together with the set's contents.  If the set's
;;; contents are further sets, then we simply take their hashes. If the set's
;;; contents are problems, then we hash each problem individually.
;;;
;;; We introduce a further new field, historic-hashes: a list of hashes, which
;;; starts out empty. Every time a module is installed into a library that
;;; contains that module, the hash of each set in that existing module is
;;; extracted and inserted at the beginning of the new version's historic-hash
;;; field.
;;;
;;; As a result we can do away with minhashes: upgrade paths are determined
;;; entirely by a set's current hash and it's historic hashes.  Upgrade maps
;;; can be described through these semantics too.
;;;
;;; It will be the responsibility of the file-store component to implement
;;; appropriate 'install', 'delete', 'compile' procedures, which implement the
;;; above features reliably.

;;; Exported, not used here
(define (hashtree? obj)
  "Return #t if OBJ is a hashtree, #f otherwise.

A hashtree is a list with a pair of the form '(blobhash . properties)as its
car, and either '() or a list containing hashtrees as its cdr."
  (match obj
    ((((? blobhash?) . (? list?)) ((? hashtree?) ...)) #t)
    ((((? blobhash?) . (? list?))) #t)
    (_ #f)))

(define (make-hashtree set)
  "Return a hashtree, starting with SET."
  (cond ((rootset? set)
         (list (cons (rootset-hash set)
                     (set-properties set))))
        (else (list (cons (set-fullhash set)
                          (set-properties set))
                    (map make-hashtree (set-contents set))))))

;;; Exported, used here
(define (set-fullhash set)
  "Return a sha256 hash of the product of recursively concatenating
all of SET's children."
  (define (hashtraverse-set set)
    (cond ((rootset? set)
           (symbol->string (rootset-hash set)))
          ((set? set)
           (apply sha256-symbol
                  (cons (symbol->string (set-id set))
                             (map hashtraverse-set
                                  (set-contents set)))))
          (else
           (error "HASHTRAVERSE-SET -- SET is not a set" set))))
  (hashtraverse-set set))

;; used here.
(define (crownset-minhash set)
  "Return a sha256 hash of SET's creator prefixed with its id."
  (sha256-symbol (string-append (symbol->string (set-id set))
                                (set-creator set))))

(define (crownset-hash-index set)
  "Return a list containing pairs of the form '(fullhash . set) for  SET and
every set referred to in SET's contents field."
  (define (minor-index set index)
    (cond ((rootset? set)
           (cons (cons (rootset-hash set) set) index))
          (else (cons (cons (set-fullhash set) set)
                      (fold minor-index index (set-contents set))))))

  (cons (cons (set-fullhash set) set) (fold minor-index '() (set-contents set))))

(define (rootset-hash set)
  "Return a sha256 hash of the set-id + the question, solution and
options fields of every problem in set-contents."
  (define (problem-composite problem)
    (string-format "~a~a~a"
                   (object->string (q-text (problem-q problem)))
                   ;; FIXME: fix re: no solution: info
                   (let ((solution (problem-s problem)))
                     (cond ((and solution (list? solution))
                            (string-join (map s-text solution)))
                           ((and solution (s? solution))
                            (object->string (s-text solution)))
                           ((not solution)
                            "false")
                           (else
                            (error "rootset-hash -- solution."))))
                   (string-join (map (lambda (option)
                                       (object->string
                                        (o-text option)))
                                     (problem-o problem)))))
  (apply sha256-symbol (cons (symbol->string (set-id set))
                             (map problem-composite
                                  (set-contents set)))))

;;; library-story.scm ends here
