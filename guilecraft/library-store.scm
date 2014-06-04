;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;;; Library Store — Library → Filesystem Interface

;; Copyright © 2012, 2014 Alex Sassmannshausen
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;;; Commentary:
;;;
;;; Provide functions to interface between the Guilecraft Library and
;;; the underlying filesystem.
;;;
;;; Functionality covers two areas:
;;; 1) Manage loading of modules from filesystem into Guilecraft
;;; 2) Provide functionality to uniformly retrieve any data from
;;;    Guilecraft sets.
;;;
;;;; Code:

(define-module (guilecraft library-store)
  #:use-module (guilecraft base32)
  #:use-module (guilecraft config)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft hash)
  #:use-module (guilecraft monads)
  #:use-module (guilecraft utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 vlist)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (import-module
            export-module
            remove-module
            compile-library
            library-hash

            fetch-set
            set-fullhash
            set-hashpairs
            known-crownsets
            search-sets
            set-details
            crownset-hashmap
            ))

;; A library is a database of all known sets indexed by their
;; fullhashes. A secondary index is stored in reference: minhash to
;; fullhash. The latter can be used to ensure continuity even after
;; set upgrades.
(define-record-type <library>
  (library catalogue reference)
  library?
  (catalogue library-cat)
  (reference library-ref))
(define (empty-library)
  (library vlist-null vlist-null))
(define (pretty-print-library lib)
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
(format #t "~a\n*Catalogue*:\n~a\n*Catalogue End*\n*Reference*:\n~a\n*Reference End*\n"
        lib
        (string-join (vlist->list (comp-string (library-cat lib) #t))
                     "\n")
        (string-join (vlist->list (comp-string (library-ref lib)))
                     "\n")))

;; A shill is a talkative value: it either grants logger premission to
;; interrogate its contents (value) or it provides a custom
;; informative message. Occasionally a programmer may not wish a shill
;; to “Spill the beans” — it won't if beans is #f.
(define-record-type <shill>
  (mecha-shill value source urgency beans)
  shill?
  (value   shill-value)
  (source  shill-source)
  (urgency shill-urgency)
  (beans   shill-beans))
(define* (shill value #:optional (source #f)
                (urgency 'debug) (beans #t))
  "Return a <shill> with beans set to #t, source to #f, and urgency to
'debug by default."
  (mecha-shill value source urgency beans))

(define (spill shill)
  (let ((urgency (shill-urgency shill))
        (beans   (shill-beans   shill)))
    (if (and beans (relevant? urgency))
        (let ((msg    (if (boolean? beans)
                          (string-append "We have: "
                                         (object->string (shill-value shill)))
                          beans))
              (source (if (shill-source shill)
                          (shill-source shill)
                          "NOT PROVIDED"))
              (port   (if (string? %log-file%)
                          (open-file %log-file% "a")
                          (current-output-port))))
          (format port "[~a]: ~a. Its source was '~a'.\n"
                  urgency msg source)
          (if (string? %log-file%) (close-output-port port))))))

(define (string-format msg . args)
  (with-output-to-string
    (lambda () (apply format #t msg args))))

;;;; Store Monad
;; A specialised monad providing logging, exception (FIXME: and file
;; locking) management.

(define (store-return . args)
  "Return a store mvalue seeded with ARGS."
  (lambda ()
    "Return a shill created using args."
    (apply shill args)))

(define (store-bind mvalue mproc)
  "Return a store mvalue, in turn capable of returning the result of
applying MVALUE to MPROC."
  (lambda ()
    (let ((shill (mvalue)))             ; extract shill
      (spill shill)
      (if (nothing? (shill-value shill))
          shill
          ((mproc (shill-value shill)))))))

(define-monad store-monad
  (bind   store-bind)
  (return store-return))

;;;; Store API
;; Any compiled-store dependent procedure (virtually all in the
;; library) require first a call to the "compile-library"
;; procedure. This is a lazy procedure that, given a store hash,
;; returns either a memoized store or compiles a store a-fresh when
;; the value is required.

(define (library-hash library-dir)
  "Return a hash, representing the state of LIBRARY-DIR."
  (cons library-dir
        (call-with-values open-sha256-port
          (lambda (port reader)
            (write (file-system-tree library-dir) port)
            (close-port port)
            (reader)))))

;; 1) Determine all module names in module dir
;; 2a) Load every module into a vhash with key: full-hash (crown-hash):
;;     a) min-hash
;;     b) (crown)set-record (new field: upgrade-map)
;; 2b) Load every module into a vhash with key: min-hash
;;     a) full-hashes
(define compile-library
  (let ((hash           (make-bytevector 0))
        (cached-library (empty-library)))
    (lambda (library-hash-pair)
      "Return library (a vhash) corresponding to the file-system state
represented by LIBRARY-HASH-PAIR."
      (let ((library-dir  (car library-hash-pair))
            (library-hash (cdr library-hash-pair)))

        (define (compile library-dir)
          "Return the library vhash from the sets in LIBRARY-MODULES."
          (define (sets-from-module module)
            "Return all sets defined in MODULE."
            (module-map (lambda (name value)
                          name ;; ignored
                          (variable-ref value))
                        module))

          (fold (lambda (module library)
                  (fold library-cons
                        library                     ; library thus far
                        (sets-from-module module))) ; these sets
                (empty-library)                     ; new library
                (library-modules library-dir)))     ; set src files

        (if (bytevector=? library-hash hash)
            cached-library
            (begin (set! hash           library-hash)
                   (set! cached-library (compile library-dir))
                   cached-library))))))

(define (library-cons set lib)
  "Return a new library consisting of LIBRARY augmented by SET."
  (let ((fullhash (set-fullhash set))
        (minhash  (crownset-minhash  set))
        (cat      (library-cat  lib))
        (ref      (library-ref  lib)))
    (library (fold (lambda (hash-set-pair cat-so-far)
                     (if (libv-assoc (car hash-set-pair) cat-so-far)
                         cat-so-far
                         (libv-cons (car hash-set-pair)
                                    (cdr hash-set-pair)
                                    cat-so-far)))
                   cat
                   (crownset-hash-index set))
             (if (libv-assoc minhash ref) ; if minhash exists, append
                 (let ((kv-pair (libv-assoc minhash ref)))
                   (libv-cons minhash (cons fullhash (cdr kv-pair))
                              (libv-delete minhash ref)))
                 (libv-cons minhash (list fullhash) ref)))))

(define (libv-cat library-pair)
  "Return the catalogue of sets derived from LIBRARY-PAIR."
  (library-cat (compile-library library-pair)))
(define (libv-ref library-pair)
  "Return the min-hash->full-hash catalogue derived from
LIBRARY-PAIR."
  (library-ref (compile-library library-pair)))

(define (fetch-set hash library-pair)
  "Return the set identified by HASH from the library derived from
LIBRARY-PAIR."
  (let ((set-pair? (libv-assoc hash (libv-cat library-pair))))
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
  "Return a list of all known crownset hashes in libv-ref derived
from LIBRARY-PAIR."
  (flatten (vlist->list (vlist-map cdr
                                   (libv-ref library-pair)))))

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

;;;;; Composite Transactions
(define (import-module filename)
  (define (import)
    ((mlet* store-monad
            ((path         (get-path filename))
             (valid-module (verify path))
             (backed-up    (move  path #t #t)) ; side-effects
             (result       (install path)))
            (return result))))
  (not (nothing? (shill-value (import)))))

(define (remove-module filename)
  (define (remove)
    ((mlet store-monad
           ((result (move filename #t)))
           (return result))))
  (not (nothing? (shill-value (remove)))))

(define* (export-module filename
                        #:optional (target %wip-library-dir%))
  (let ((target (if (string=? target "./")
                    (getenv "PWD")
                    target)))
    (define (export)
      ((mlet store-monad
             ((result (move filename #:target target)))
             (return result))))
    (not (nothing? (shill-value (export))))))

;;;;; Atomic Transactions / Monadic Transactions
(define (get-path filename)
  (apply store-return
   (cond ((and (char=? (string-ref filename 0) #\/)
               (test-file filename))
          ;; Filepath is absolute
          (list filename))
         ((test-file filename)
          ;; Filepath is relative
          (list (string-format "~a/~a" (getenv "PWD") filename)))
         ((test-file (string-format "~a/~a"
                                    %wip-library-dir% filename))
          ;; File's in %wip-library-dir%
          (list (string-format "~a/~a" %wip-library-dir% filename)))
         ;; File does not exist.
         (else
          (list (nothing 'import-error filename) 'get-path 'error
                (string-format
                 "~a is not a regular file or does not exist"
                 filename))))))

(define* (move filename #:optional deletion? backup?
                #:key (target %bak-library-dir%))
  (apply store-return
         (let* ((name   (basename filename))
                (module (string-format "~a/~a" %library-dir% name))
                (proc   (if deletion? rename-file copy-file)))
           (if (test-file module)
               (catch 'system-error
                 (lambda ()
                   (proc module (string-format "~a/~a" target name))
                   (list filename 'move 'inform
                         (cond ((not (string=? target
                                               %bak-library-dir%))
                                (string-format "~a: exported to ~a"
                                               name target))
                               ((and deletion? (not backup?))
                                (string-format "~a: deleted" name))
                               (else (string-format "~a: backed up"
                                                    name)))))
                 (lambda (k . a)
                   (list (nothing k name) 'move 'error
                         (cond ((not (string=? target
                                               %bak-library-dir%))
                                (string-format
                                 "~a: error exporting to ~a" name
                                 target))
                               ((and deletion? (not backup?))
                                (string-format "~a: error deleting"
                                               name))
                               (else (string-format
                                      "~a: error backing up"
                                      name))))))
               (cond ((not (string=? target %bak-library-dir%))
                      (list (nothing 'export-error '(name target))
                            'move 'error
                            (string-format
                             "~a:  error exporting to ~a" name
                             target)))
                     ((and deletion? (not backup?))
                      (list (nothing 'deletion-error name)
                            'move 'error
                            (string-format "~a: file not found"
                                           name)))
                     (else (list filename 'move 'inform
                                 (string-format
                                  "~a: ~a, ~a"
                                  name "file not found"
                                  "hence not backed up"))))))))

(define (verify filename)
  (apply store-return
         (let ((name (basename filename)))
           (catch 'system-error
             (lambda ()
               (load filename)
               (list filename 'verify 'inform
                     (string-format "~a: has been verified" name)))
             (lambda (k . a)
               `(,(nothing k name) verify error
                 ,(string-format "~a: could not be verified"
                                 name)))))))

(define (install filename)
  (apply store-return
         (let ((name (basename filename)))
           (catch 'system-error
             (lambda ()
               (copy-file filename
                          (string-format "~a/~a" %library-dir% name))
               (list filename 'install 'inform
                     (string-format "~a: has been installed" name)))
             (lambda (k . a)
               `(,(nothing k name) install error
                 ,(string-format "~a: could not be installed"
                                 name)))))))

;;;;; Helpers
(define (test-file filename)
  (catch 'system-error
    (lambda () (eqv? (stat:type (stat filename)) 'regular))
    (lambda (key . args) #f)))

(define (libv-cons key value vhash)
  (vhash-cons key value vhash))
(define (libv-assoc key vhash)
  (vhash-assoc key vhash))
(define (libv-delete key vhash)
  (vhash-delete key vhash))
(define (libv-fold proc init vhash)
  (vhash-fold proc init vhash))

(define (library-modules library-dir)
  "Return the list of modules that provide packages for the distribution."
  (define not-slash
    (char-set-complement (char-set #\/)))
  (define (data-files library-dir)
    "Return the list of files that implement guilecraft modules."
    (define prefix-len
      (string-length
       (dirname library-dir)))

    (file-system-fold (const #t)               ; enter?
                      (lambda (path stat result) ; leaf
                        (if (string-suffix? ".scm" path)
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
                      library-dir
                      stat))

  (filter-map (lambda (path)
                (let ((name (map string->symbol
                                 (string-tokenize (string-drop-right path 4)
                                                  not-slash))))
                  (if (not (%search-load-path (symbol->string (cadr name))))
                      (add-to-load-path (dirname library-dir)))
                  (false-if-exception (resolve-interface name))))
              (data-files library-dir)))


;;;;; Procedures for inclusion in Set
;; FIXME: hash functions should work with promises?
(define (crownset-minhash set)
  "Return a sha256 hash of SET's creator prefixed with its id."
  (sha256-symbol (string-append (symbol->string (set-id set))
                                (set-creator set))))

(define (crownset-hash-index set)
  "Return a list containing pairs of every set-fullhash and set
contained referred to by SET."
  (define (minor-index set index)
    (cond ((rootset? set)
           (cons (cons (rootset-hash set) set) index))
          (else (cons (cons (set-fullhash set) set)
                      (fold minor-index index (set-contents set))))))

  (cons (cons (set-fullhash set) set) (fold minor-index '() (set-contents set))))

(define (crownset-hashmap set)
  (define (hashtraverse-set set)
    "Return a set-hashmap, a list containing lists of blobhashes for
each set and it's children that this hashtraverser is passed."
    (cond ((problem? (car (set-contents set)))
           (list (rootset-hash set)))
          (else (list (set-fullhash set)
                      (map hashtraverse-set
                           (set-contents set))))))
  (hashtraverse-set set))

(define (set-fullhash set)
  "Return a sha256 hash of the product of recursively concatenating
all of SET's children."
  (define (hashtraverse-set set)
    (cond ((problem? (car (set-contents set)))
           (symbol->string (rootset-hash set)))
          (else (apply sha256-symbol
                       (cons (symbol->string (set-id set))
                             (map hashtraverse-set
                                  (set-contents set)))))))
  (hashtraverse-set set))

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
