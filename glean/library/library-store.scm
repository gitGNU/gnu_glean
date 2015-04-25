;; library-store.scm --- library → filesystem interface   -*- coding: utf-8 -*-
;;
;; Copyright (C) 2012, 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
;;
;; This file is part of Glean.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
  #:use-module (glean library lexp)
  #:use-module (glean library sets)
  #:use-module (glean library set-tools)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (
            compile-library
            catalogue-hash

            fetch-set
            fetch-set-from-lexp
            set-hashpairs
            crownsets
            known-crownsets
            search-sets
            set-details
            ))


;;;;; The Library
;;;
;;; A library is a database of two tables:
;;; 1) All known modules are indexed by their fullhashes (=> shallowhash)
;;; 2) A secondary reference from minhash to fullhash.  The latter can contain
;;;    one to many mappings and can be used to ensure continuity even after
;;;    module upgrades.
;;; 2) Revised: A secondary reference of discipline-lexp -> dag-hash.  This
;;;    can be used, in conjunction with the four fields provided in a request
;;;    (counter, shallowhash, dag-hash & base-lexp), to generate an upgrade
;;;    map on the basis of a shallowhash that has been superseded.
;;;
;;; A shallowhash is the hash of a set's lexp, version and the lexps of its
;;; direct descendants, concatenated.
;;;
;;; A daghash is a hash capituring the full dag of a single discipline.
;;;
;;; For more details on hashes see (glean libarry set-tools).

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

(define* (pretty-print-library lib #:key (port #t))
  "Print the library LIB to PORT, defaulting to current-output-port."
  (define (lexp->string lxp)
    (string-join (map symbol->string (lexp-serialize lxp)) "::"))
  ;; library-cat: '($shallowhash . ((set . $set) (lexp . $lexp)))
  (define (cat-string entry)
    (match entry
      ((hash . (('set . set) ('lexp . lxp)))
       (string-append "  Set name: " (set-name set)
                      "\n  Lexp: " (lexp->string lxp)
                      "\n  Hash: " hash
                      "\n  ---"))))
  ;; library-ref: '($lexp . $set)
  (define (ref-string entry)
    (match entry
      ((lxp . set)
       (string-append "  Set name: " (set-name set)
                      "\n  Lexp: " (lexp->string lxp)
                      "\n  ---"))))
  (format port "~a~%Catalogue:~%~a~%Reference:~%~a~%"
          lib
          (string-join (vlist->list (vlist-map cat-string (library-cat lib)))
                       "\n")
          (string-join (vlist->list (vlist-map ref-string (library-ref lib)))
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
    (lambda* (catalogue-hash-pair #:key crass?)
      "Return library (a vhash) corresponding to the file-system state
represented by CATALOGUE-HASH-PAIR.  If the hash in CATALOGUE-HASH-PAIR was
passed to us before, return the previously computed library.  Else,
re-compute."
      (match catalogue-hash-pair
        ((catalogue-dir . catalogue-hash)
         (define (compile)
           "Return the library vhash from the sets in LIBRARY-MODULES."
           (fold library-add-disciplines
                 (empty-library)
                 (discipline-modules catalogue-dir #:crass? crass?)))

         ;; If CATALOGUE-HASH has not changed, return cached library.
         (if (bytevector=? catalogue-hash cached-hash)
             cached-library
             (begin
               (set! cached-library (empty-library))
               (set! cached-library (compile))
               (set! cached-hash    catalogue-hash)
               cached-library)))
        (#f cached-library)))))


;;;;; Library Convenience

(define (library-add-disciplines discipline-module library)
  (fold library-cons library (sets-from-module discipline-module)))

(define (library-cons discipline current-library)
  "Return a new library consisting of CURRENT-LIBRARY augmented by DISCIPLINE.

In practice this means that we:
- add DISCIPLINE to library's Reference '(base-lexp -> discipline)
- recursively add discipline and all subsets to library's Catalogue
  '(shallow-hash -> set)"

  (define (add-if-missing entry index)
    "Return index, augmented by ENTRY if it does not yet exist in INDEX."
    (match entry
      ((shallow-hash set lxp)
       (if (lib-assoc shallow-hash index)
           index              ; set added previously
           (lib-cons shallow-hash `((set . ,set) (lexp . ,lxp)) index)))
      (_ (throw 'glean-type-error 'add-if-missing
                "Expected pair, got:" entry))))

  (let ((lxp (set-lexp discipline)))
    (match current-library
      (($ <library> index reference)
       (library (fold add-if-missing index (index-set discipline lxp))
         (if (lib-assoc lxp reference)
             ;; This should only happen if a discipline loaded
             ;; into library at an earlier time uses the same
             ;; base-lexp, which is not supported.
             (throw 'glean-logic-error 'library-cons
                    "Duplicate base-lxp entry!")
             (lib-cons lxp discipline reference))))
      (_ (throw 'glean-type-error 'library-cons
                "Expected <library>, got:" library)))))

(define (lib-cat library-pair)
  "Return the catalogue of sets derived from LIBRARY-PAIR."
  (library-cat (compile-library library-pair)))

(define* (lib-ref library-pair #:key crass?)
  "Return the min-hash->full-hash catalogue derived from
LIBRARY-PAIR."
  (library-ref (compile-library library-pair #:crass? crass?)))

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

(define* (fetch-set hash library-pair #:optional full?)
  "Return the set identified by HASH from the library derived from
LIBRARY-PAIR."
  (match (lib-assoc hash (lib-cat library-pair))
    (((? hash? hash) . set-assoc)
     (cond ((eqv? full? 'sets) (cdar set-assoc))
           (full? `(,hash . ,set-assoc))
           (else set-assoc)))
    (#f #f)))

(define* (fetch-set-from-lexp lxp library-pair #:key crass?)
  "Return the set identified by LXP from the library derived from
LIBRARY-PAIR.  If we cannot find the set, return #f."
  (match (lib-assoc lxp (lib-ref library-pair #:crass? crass?))
    ((($ <lexp>) . set)
     set)
    (#f #f)))

(define (known-crownsets library-pair config-ignore-keywords)
  "Return a list containing summary information on all known crownsets
in the library derived from LIBRARY-PAIR, ignoring those that contain
keywords blacklisted in CONFIG-IGNORE-KEYWORDS."
  (fold (lambda (entry sets)
          (match entry
            ((lxp . set)
             (if (null? (lset-intersection string=?
                                           config-ignore-keywords
                                           (set-keywords set)))
                 (cons (set-summary set (shallow-hash lxp set)) sets)
                 sets))))
        '()
        (vlist->list (lib-ref library-pair))))

(define (search-sets operator search library-pair)
  "Return a list containing summary information for all sets matching
the criteria specified by OPERATOR and SEARCH in LIBRARY-PAIR."
  (define (search-by-hash hashes)
    "Return the set summary of any sets identified by one of the
hashes in HASHES."
    (map (lambda (entry)
           (match entry
             ((hash . '(('set . set) ('lexp . lxp)))
              (set-summary set hash))))
         (filter-map (cut fetch-set <> library-pair #t)
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

(define* (set-summary set hash)
  "Return a list containing summary information on SET."
  (list hash
        (set-id       set)
        (set-name     set)
        (set-version  set)
        (set-keywords set)
        (set-synopsis set)
        (set-logo     set)))

(define (set-details hash library-pair)
  "Return a list containing detailed information on SET, or #f if it
cannot be found in LIBRARY-PAIR."
  (define (summarize child lxp)
    (set-summary child
                 (shallow-hash (lexp-append lxp (set-id child)) child)))

  (match (fetch-set hash library-pair)
    ((('set . set) ('lexp . lxp))
     (list hash
           (set-id set)
           (set-name set)
           (set-version set)
           (set-keywords set)
           (set-synopsis set)
           (set-description set)
           (set-creator set)
           ;; FIXME: I've disabled passing back of media for now — they need
           ;; to be reworked anyway.
           'resources
           'attributes
           (set-properties set)
           (if (rootset? set)
               '()
               (map (cut summarize <> lxp) (set-contents set)))
           (set-logo set)))
    (#f #f)))


(define (crownsets library-pair)
  "Return the list of disciplines currently stored in the library identified
by LIBRARY-PAIR."
  (vlist->list (vlist-map cdr (lib-ref library-pair))))

(define (set-hashpairs fullhashes library-pair)
  "Return a list of (minhash . fullhash) for each hash in FULLHASH, if
it is known in LIBRARY-PAIR."
  (map (lambda (fullhash)
         (match (fetch-set fullhash library-pair)
           ((('set . set) ('lexp . lxp))
            (cons (lexp-serialize lxp) fullhash)) ; found
           (#f
            (cons #f fullhash))))       ; invalid fullhash
       fullhashes))


;;;;; Helpers

(define (string-format msg . args)
  "Return a string composed of MSG and ARGS, by constructing it using format."
  (with-output-to-string
    (lambda () (apply format #t msg args))))

(define (sets-from-module module)
  "Return a list containing each exported set in MODULE.  Ignore all exported
bindings which are not sets.

Should module not export any bindings, raise an error."
  (define (verbose-discipline-filter maybe-discipline)
    (match maybe-discipline
      ((? plain-module? discipline) discipline)
      (UFO (caution "~a: not a discipline, eliminating.~%"
                    (cond ((set? UFO) (set-name UFO))
                          ((nothing? UFO) 'nothing)
                          (else UFO)))
           #f)))
  (define (verbose-set-resolver name variable)
    (inform "Loading ~a..." name)
    (match (resolve-set (variable-ref variable))
      ((? set? set)
       (inform "[success]~%") set)
      (UFO (caution "[failure]~%")
           set)))

  (or (and=> (module-public-interface module)
             (lambda (public-interface)
               (filter verbose-discipline-filter
                       (module-map verbose-set-resolver public-interface))))
      (error "SETS-FROM-MODULE -- module did not resolve:" module)))

(define* (discipline-modules catalogue-dir #:key crass?)
  "Return the list of Guile modules, derived from the files in CATALOGUE-DIR,
that provide content for the library. These modules are then parsed using
`sets-from-module' to retrieve the Glean modules contained therein."
  (define* (discipline-files)
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
  (define (discipline-files->modules path)
    (define not-slash (char-set-complement (char-set #\/)))
    (let ((name (map string->symbol
                     (string-tokenize (string-drop-right path 4)
                                      not-slash))))
      ;; We want to make sure that a module is reloaded from the library if it
      ;; has been loaded before, as it may have changed.
      (match (resolve-module name #:ensure #f)          ; loaded before?
        (#f (resolve-module name)) ; no  -> load
        ((? module? module)                            ; yes -> reload
         (inform "We have previously loaded '~a'.~%" name)
         (inform "Module-filename: '~a'.~%" (module-filename module))
         (when crass?
           ;; We are really messing with the dark arts here: we're trying to
           ;; force Guile to load a new discipline that is identical to an
           ;; existing discipline, but which needs to be loaded from a
           ;; different place.  This is probably done in the context of
           ;; (glean maker engrave).
           (inform "Crass is enabled, setting filename to ~a~%" (string-append catalogue-dir path))
           (set-module-filename! module (string-append catalogue-dir path)))
         (reload-module module)))))
  (begin (add-to-load-path catalogue-dir)
         (let ((rslt (filter-map discipline-files->modules (discipline-files))))
           (set! %load-path (cdr %load-path))
           rslt)))


(define* (index-set set #:optional (lxp (set-lexp set)))
  "Return a list of '(shallow-hash . set) for SET and recursively all its
children.  If LXP is not provided then we assume that SET is a crownset, and
generate its base-lexp."

  (define* (index set lxp #:optional (indx '()))
    "Return INDX after appending a pair of form '(shallow-hash set lxp) to it.
shallow-hash is composed of LXP and SET, set is SET and lxp is LXP."
    (cons (list (shallow-hash lxp set) set lxp) indx))

  (define (index-child child indx)
    "Return INDX after augmenting it by recursively indexing CHILD and
appending the resulting index."
    (if (rootset? child)
        (index child (lexp-append lxp (set-id child)) indx)
        (append (index-set child (lexp-append lxp (set-id child)))
                indx)))

  (fold index-child (index set lxp) (set-contents set)))

;;; library-story.scm ends here
