;; catalogues.scm --- low-level catalogue manipulation   -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 18 November 2014
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
;; Catalogues are snapshots of the library store.  They are implemented as
;; symlink forests.
;;
;; Each catalogue is represented in the filesystem as a directory containing
;; symlinks to disciplines installed in the store.
;;
;; Within Glean, catalogues are first and foremost a record type. The record
;; contains all information necessary for interacting with catalogues (reading
;; catalogues from the filesystem, writing new catalogues, etc.).
;;
;; An additional symlink, current-catalogue, stored in the `library-dir', is
;; used to always point to the catalogue currently in use by `glean library'.
;;
;; The current-catalogue is the data-store actually used by the library at any
;; given point.  That is, the library normally never has a complete view of
;; all the disciplines in the store, only of those in the `current-catalogue.'
;;
;; Procedures interacting with the file-system itself, i.e. IO procedures, are
;; named actively (e.g. catalogue-install -> catalogue-installer).
;;
;;; Code:

(define-module (glean librarian catalogues)
  #:use-module (glean common config-utils)
  #:use-module (glean common monads)
  #:use-module (glean common utils)
  #:use-module (glean library library-store)
  #:use-module (glean library sets)
  #:use-module (glean library set-tools)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export     (catalogue
                catalogue?
                get-catalogue-id
                get-disiciplines
                get-catalogue-dir
                catalogue-list
                catalogue-remove
                catalogue-show
                catalogue-install
                mcatalogue-tmp))


;;;; Catalogue Record Type Definition

(define-immutable-record-type catalogue
  (make-catalogue id disciplines cat-dir)
  catalogue?
  (id          get-catalogue-id)
  (disciplines get-disciplines set-disciplines)
  (cat-dir     get-catalogue-dir))

;;; Records used in Catalogue related file system traversal.

(define-immutable-record-type journey
  (make-journey depth-gauge journey-log cat-dir)
  journey?
  (depth-gauge get-depth set-depth)
  (journey-log get-log   set-log)
  (cat-dir     get-cat))


;;;; UI
;;;
;;; High level UI procedures.  These should be the only procedures emitting
;;; output directly to the user.
;;; These procedures are also the points of entry for this module as used by
;;; `(glean librarian boot)'.

(define (catalogue-install cat-dir curr-cat lib-dir target)
  "Attempt to install TARGET in the store at LIB-DIR, create a new catalogue
in CAT-DIR and update CURR-CAT.  We will exit with an error if we encounter a
problem.

CAT-DIR:  string pointing to the directory in which catalogues are to be
          created.
CURR-CAT: string pointing to the current-catalogue symlink.
LIB-DIR:  string pointing to the directory into which disciplines can be
          installed (store).
TARGET:   string pointing to a discipline directory.
"
  (match (mcatalogue-install cat-dir curr-cat lib-dir target)
    ((? nothing? nothing)
     (emit-error (nothing-id nothing) (nothing-context nothing)))
    ((? catalogue? catalogue)
     (emit-catalogue catalogue))))

(define (catalogue-remove cat-dir curr-cat disc-id)
  "Attempt to create and activate a new catalogue with the discipline
identified by DISC-ID removed from it.  We will exit with an error if we
encounter a problem.

CAT-DIR:  string pointing to the directory in which catalogues are to be
          created.
CURR-CAT: string pointing to the current-catalogue symlink.
DISC-ID:  string corresponding to a disciplines id (#:id & directory-name).
"
  (match (mcatalogue-remove cat-dir curr-cat disc-id)
    ((? nothing? nothing)
     (emit-error (nothing-id nothing) (nothing-context nothing)))
    ((? catalogue? catalogue)
     (emit-catalogue catalogue))))

(define (catalogue-show cat-dir cat-id)
  "Attempt to show details about catalogue CAT-ID stored in CAT-DIR.  Exit
with an error if we encounter a problem.

CAT-DIR:  string pointing to the directory in which catalogues are to be
          created.
CAT-ID:   string naming the catalogue as installed in CAT-DIR.
"
  (match (mcatalogue-show cat-dir cat-id)
    ((? nothing? nothing)
     (emit-error (nothing-id nothing) (nothing-context nothing)))
    ((? catalogue? catalogue)
     (emit-catalogue catalogue))))

(define (catalogue-list cat-dir)
  "Attempt to list all catalogues stored in CAT-DIR.  Exit with an error if we
encounter a problem.

CAT-DIR:  string pointing to the directory in which catalogues are to be
          created.
"
  (match (mcatalogue-list cat-dir)
    ((? nothing? nothing)
     (emit-error (nothing-id nothing) (nothing-context nothing)))
    (((? catalogue? catalogue) ...)
     (for-each (lambda (cat) (emit-catalogue cat #:full? #f))
               catalogue))))

(define* (emit-error id #:optional (context #f))
  "Emit an error message corresponding to ID and exit."
  (match id
    ('catalogue-detailer
     (match context ((cat-id . cat-dir)
                     (leave (_ "Catalogue '~a' could not be found in ~a.~%")
                            cat-id cat-dir))))
    ('catalogue-installer
     (leave (_ "A problem occured creating the new catalogue.\n")))
    ('catalogue-lister
     (match context
       ((cat-dir . #f) (leave (_ "No catalogues found at ~a.~%") cat-dir))))
    ('current-catalogue-setter
     (leave (_ "A problem occured switching to the catalogue.\n")))
    ('current-catalogue-namer
     (leave (_ "Unable to establish current catalogue's name.\n")))
    ('discipline-installer
     (match context
       (('duplicate . target)
        (leave (_ "A discipline with the unique name~%\t'~a'
already exists in the store.~%
Instead of installing the discipline again, you can simply re-activate it.
Alternatively, if you know what you are doing, delete the discipline from the
store and try again.~%")
               target))
       (_
        (leave (_ "We encountered a problem installing the discipline.~%")))))
    ('invalid-catalogue-store
     (match context
       ((path) (leave (_ "Invalid catalogue found at ~a.~%") path))))
    ('next-catalogue-counter-maker
     (leave (_ "A problem occured creating a new counter.")))))

(define* (emit-catalogue cat #:key (full? #t))
  "Emit a summary of the catalogue CAT, listing CAT's id and the id of each
discipline linked in it.  If FULL? is #t, also emit the mapping for each
discipline id to the discipline in the store."
  (match cat
    (($ catalogue id disciplines)
     (format #t "~a (~a)~%"
             id
             (string-join
              (vlist->list
               (vlist-map (lambda (discipline)
                            (match discipline
                              ((name . target)
                               (if full?
                                   (string-append name " => "
                                                  target)
                                   name))))
                          disciplines))
              ", ")))))


;;;; Library/Catalogue Intersection
;;;
;;; This procedure is special: it makes use of procedures defined in `(glean
;;; library library-store)', as it creates a temporary library to retrieve
;;; details about the set identified by SET-ID, which can only be retrieved
;;; once set is loaded in the context of a library.
;;;
;;; This procedure will allow us to resolve lexps against actual content in
;;; the library for instance.

(define (store-name tmp-lib id)
  "Return the 'store-name', a string of the format \"$hash-$id-$version\", for
the discipline identified by set-id ID as found in the library built out of
the store pointed towards by tmp-lib, a library-hash-pair.

See (glean library library-store) for more details on the latter
data-structure.

TMP-LIB: a library structure (glean library library-store).
ID:      a symbol naming a discipline through its #:id.
"
  (let ((discipline (fold (lambda (set result)
                            (cond (result result)
                                  ((eqv? (set-id set) id) set)
                                  (else #f)))
                          #f
                          (crownsets tmp-lib))))
    (string-join `(,(deep-hash discipline)
                   ,(symbol->string id)
                   ,(set-version discipline))
                 "-")))


;;;; Atomic Catalogue Operations

(define (make-bare-catalogue cat-id cat-dir)
  "Return a fresh catalogue, with CAT-ID as the catalogue's id and CAT-DIR as
its catalologue-dir.

CAT-ID:   string naming the catalogue as installed in CAT-DIR.
CAT-DIR:  string pointing to the directory in which catalogues are to be
          created.
"
  (make-catalogue cat-id vlist-null cat-dir))

(define* (catalogue-add-discipline cat disc-pair #:optional new-id new-dir)
  "Generate a new catalogue based on catalogue CAT, augmented by the
discipline identified by pair DISC-PAIR of the form (discipline-id . path).

If DISC-PAIR's id was already part of CAT, simply update that discipline's
target to the one provided by DISC-PAIR.

If NEW-ID is provided, use this as the name of the resulting catalogue; if
NEW-DIR is provided, use this as the new directory associated with the new
catalogue.

CAT:       <catalogue> to which the discipline will be added.
DISC-PAIR: pair of strings, the discipline's ID as a string, and a pointer to
           its location in the store.
NEW-ID:    either a string containing the new name for the catalogue, or #f.
NEW-DIR:   either a string pointing to the catalogue directory containing this
           catalogue or #f.
"
  (match cat
    (($ catalogue id disciplines dir)
     (match disc-pair
       ((name . target)
        (make-catalogue (or new-id id)
                        (if (vhash-assoc name disciplines)
                            (vhash-fold (lambda (fold-name fold-target result)
                                          (if (string=? fold-name name)
                                              (vhash-cons name target result)
                                              (vhash-cons fold-name
                                                          fold-target
                                                          result)))
                                        vlist-null
                                        disciplines)
                            (vhash-cons name target disciplines))
                        (or new-dir dir)))))
    (_ (throw 'glean-type-error 'catalogue-add-discipline cat
              "Expected: <catalogue>"))))

(define* (augment-catalogue cat counter name filename #:optional tmp)
  "Return a new catalogue, incorporating the disciplines of catalogue CAT,
named with COUNTER, and augmented by FILENAME.

If tmp is provided it should be a string to a temporary directory.  This is
provided for the creation of temporary catalogues.

CAT:            <catalogue> to which the discipline will be added.
COUNTER:        number identifying the suffix of the revised catalogue.
NAME:           string identifying the catalogue name for the discipline.
FILENAME:       string pointing to the discipline to be added in the store.
TMP:            string pointing to an alternative catalogue directory or #f.
                This is used when working with a temp-library.
"
  (catalogue-add-discipline cat
                            (cons name filename)
                            (make-catalogue-name counter)
                            tmp))

(define (impair-catalogue cat counter id)
  "Return a new catalogue, incorporating all disciplines from catalogue CAT,
except for the one identified by set-id ID.  This new catalogue will be named
using COUNTER.

CAT:     <catalogue> to which the discipline will be added.
COUNTER: number identifying the suffix of the revised catalogue.
ID:      string naming the discipline to be deleted by its #:id.
"
  (match cat
    (($ catalogue name disciplines dir)
     (make-catalogue (make-catalogue-name counter)
                     (vhash-delete id disciplines)
                     dir))))

;;;; Catalogue File System Semantics
;;;
;;; The Catalogue system maps to a filesystem structure.  This section maps
;;; that structure to predicates, accessors etc.

(define (relative-filename . components)
  "Return the string which consists of COMPONENTS, joined by this operating
system's filename separator.

COMPONENTS: list of string(s) to be joined."
  (string-join components file-name-separator-string))

;;; Catalogues contain disciplines which will be loaded by the library.
;;; Disciplines are loaded as normal Guile modules.  As such they want to be
;;; loaded in a namespace.  To avoid unexpected name clashes we should
;;; maintain a disciplines name space.  This namespace needs to exist as an
;;; actual directory structure in each catalogue.  The following provides
;;; such a name space for use in catalogues.
(define load-path-suffix (const (relative-filename "glean" "disciplines")))

(define (discipline? stat)
  "Disciplines in catalogue stores are identified by them being a
symlink.  Return #t if so, #f otherwise.

STAT: stat object as returned by (stat).
"
  (eqv? (stat:type stat) 'symlink))

(define (catalogue-directory cat-dir cat-id)
  "Return a catalogue directory string; a string pointing towards the
catalogue named by CAT-ID in CAT-DIR.

CAT-DIR: string pointing to the directory in which catalogues are to be
         created.
CAT-ID:  string naming the catalogue as installed in CAT-DIR.
"
  (relative-filename cat-dir cat-id))

(define (discipline-directory cat-dir cat-id)
  "Return a catalogue directory string which includes the standard glean
load-path-suffix for disciplines; a string pointing towards the catalogue
disciplines in the catalogue identified by CAT-ID in CAT-DIR.

In contrast to `catalogue-directory', this points to the folder, within a
specific catalogue, which contains disciplines.  `catalouge-directory' points
to the catalogue itself within the catalogue store.

CAT-DIR: string pointing to the catalogue directory.
CAT-ID:  string naming the catalogue as installed in CAT-DIR.
"
  (relative-filename (catalogue-directory cat-dir cat-id)
                     (load-path-suffix)))

(define (discipline-filename catalogue-dir discipline-id)
  "Return a discipline file name: a string pointing towards the discipline
named by DISCIPLINE-ID in the catalogue identified by CATALOGUE-DIR.

CATALOGUE-DIR: string pointing to the directory in which catalogues are to be
               created.
DISCIPLINE-ID: string identifying a discipline through its #:set-id.
"
  (relative-filename catalogue-dir discipline-id))

(define (make-catalogue-name counter)
  "Return a new catalogue-name of the form `catalogue-COUNTER'.

COUNTER: number.
"
  (string-append "catalogue-" (number->string counter)))

(define (cataloguename path)
  "Return the string identifying the catalogue portion of PATH or raise an
error.  This procedure works similar to basename, except it does not return
the filename part of PATH, but the part that would identify the catalogue.

PATH: string pointing to a directory containing a catalogue.
"
  (basename (string-drop-right path (string-length (load-path-suffix)))))

;;; The catalogue folder contains catalogues, which in turn contains the
;;; LOAD-PATH-SUFFIX, and finally symlinks to the disciplines in the store.
;;; As a result, any folder recursion starting at %catalogue-dir% should have
;;; a maximum depth of 3 (0 indexed).
;;;
;;; Despite the above reasoning, there are a number of hard-coded bits
;;; relating to depth & catalogue path which displease me.

(define (ok-depth? journey)
  "Return #t if the gauge in JOURNEY indicates an acceptable depth.

JOURNEY: <journey>."
  (< (get-depth journey) 4))

(define (go-deeper depth)
  "Increase the depth of the gauge in JOURNEY.

DEPTH: number identifying the number of times we've descended into directories
       since we started the traversal.
"
  (1+ depth))

(define (come-up depth)
  "Decrease the depth of the gauge in JOURNEY.

DEPTH: number identifying the number of times we've descended into directories
       since we started the traversal.
"
  (1- depth))

;;;; Journey/Filesystem Traversal Operations: I/O operations

(define* (make-bare-journey depth cat-dir #:key (state '()))
  "Return a fresh journey.

DEPTH:   number identifying the number of times we've descended into
         directories since we started the traversal.
CAT-DIR: string pointing to a directory containing catalogues.
STATE:   list containing additional information relevant to this <journey>.
"
  (make-journey depth state cat-dir))

(define (log-add-catalogue catalogue-id log catalogue-dir)
  "Return JOURNEY-LOG augmented by a fresh catalogue for CATALOGUE-ID.

CATALOGUE-DIR: string pointing to the directory in which catalogues are to be
               created.
LOG:           list containing accrued results so far, for use in <journey>s.
CATALOGUE-ID:  string naming the catalogue as installed in CATALOGUE-DIR.
"
  (cons (make-bare-catalogue catalogue-id catalogue-dir) log))

(define (log-add-discipline disc-link log)
  "Return LOG with the first catalogue in it augmented by the discipline
pointed to by DISCIPLINE-LINK.

DISC-LINK: string pointing to a symlink in a catalogue directory pointing
           towards a discipline in the store.
LOG:       list containing accrued results so far, for use in <journey>s.
"
  (match log
    ((incomplete-catalogue . rest)
     (cons (catalogue-add-discipline incomplete-catalogue
                                     (cons (basename disc-link)
                                           (readlink disc-link)))
           rest))))

(define (catalogue-leaf path stat journey)
  "Add the discipline to the skeleton and return journey, or throw an
error — if it is not a symlink we have an invalid Catalogues store.

PATH:    string pointing to the leaf currently being handled.
STAT:    object as returned by (stat).
JOURNEY: <journey> object summarizing the traversal.
"
  (if (discipline? stat)
      (set-log journey (log-add-discipline path
                                           (get-log journey)))
      (throw 'invalid-catalogue-store path)))

(define (catalogue-up path stat journey)
  "Simply return JOURNEY unchanged.

PATH:    string pointing to the leaf currently being handled.
STAT:    object as returned by (stat).
JOURNEY: <journey> object summarizing the traversal.
"
  (set-depth journey (come-up (get-depth journey))))

(define (catalogue-skip path stat journey)
  "Emit warning, as unexpected, but simply return JOURNEY.

PATH:    string pointing to the leaf currently being handled.
STAT:    object as returned by (stat).
JOURNEY: <journey> object summarizing the traversal.
"
  (warning (_ "We unexpectedly skipped a directory: ~a") path)
  journey)

(define (catalogue-error path stat errno journey)
  "Unless ERRNO is 2, which would mean that we cannot find the precise file we
are looking for, emit a warning and proceed by returning JOURNEY.  In the
former case, raise an error.

PATH:    string pointing to the leaf currently being handled.
STAT:    object as returned by (stat).
ERRNO:   number reflecting the error just encountered.
JOURNEY: <journey> object summarizing the traversal.
"
  (if (= errno 2)
      (throw 'unknown-catalogue '())
      (warning (_ "Encountered error while accessing ~a: ~a.~%") path errno))
  journey)

(define* (catalogue-system-fold local-enter? local-down init dir
                                #:key (local-leaf catalogue-leaf)
                                (local-skip catalogue-skip))
  "Perform a specialized version of file-system-fold, in which a number of the
procedures are set to procedures that make sense in the catalogues context,
and where other procedures (LOCAL-LEAF, LOCAL-SKIP) default to such that make
sense.  LOCAL-ENTER? and LOCAL-DOWN should be procedures corresponding to
`enter?' and `down' in `file-system-fold'.  INIT would normally be a journey
record; DIR would normally be the default catalogue-dir.

LOCAL-ENTER?: procedure (predicate) telling us whether to descend.
LOCAL-DOWN:   procedure telling us what to do upon descent.
INIT:         <journey> containing state to start this fold with.
DIR:          string pointing to the directory to start the fold from.
LOCAL-LEAF:   procedure telling us what to do upon hitting a leaf.
LOCAL-SKIP:   procedure telling us what to do when we do not descend.
"
  (catch #t
    (lambda ()
      (call-with-values
          (lambda ()
            ;; Catch here is because file-system-fold returns values if
            ;; successful, but will return null? if it cannot find the starting
            ;; dir.
            (catch 'vm-error
              (lambda ()
                (file-system-fold local-enter? local-leaf local-down catalogue-up
                                  local-skip catalogue-error init dir))
              (lambda args (values '() '()))))
        (lambda (result vhash)
          (if (journey? result) (get-log result) result))))
    (lambda (key . args)
      (nothing key args))))


;;;; Monadic File System Procedures

;;;;; Catalogue Listings

(define (catalogue-lister)
  "Return a procedure of one argument, a string identifying a catalogue
directory, which when applied, returns a list of catalogues found in that
directory, or a nothing with id 'catalogue-lister and information suitable for
creating output messages."
  (define (local-enter? path stat journey)
    "Return #t if we're not too deep, raise an error otherwise."
    (or (ok-depth? journey)
        (throw 'invalid-catalogue-store path)))
  (define (local-down path stat journey)
    "Upon descending in dir we want to create the template for a new
catalogue, if we find ourselves at the appropriate depth."
    (if (< (get-depth journey) 3)       ; depth inc. load-path-suffix.
        (set-depth journey (go-deeper (get-depth journey)))
        (make-journey (go-deeper (get-depth journey))
                      (log-add-catalogue (cataloguename path)
                                         (get-log journey)
                                         (get-cat journey))
                      (get-cat journey))))
  
  (lambda (catalogue-dir)
    (match (catalogue-system-fold local-enter? local-down
                                  (make-bare-journey 0 catalogue-dir)
                                  catalogue-dir)
      (() (nothing 'catalogue-lister (cons catalogue-dir #f)))
      (otherwise otherwise))))

;;;;; Catalogue Detail

(define (catalogue-detailer catalogue-id)
  "Return a procedure of one argument, a string identifying a catalogue
directory, which when applied, returns a list containing the catalogue record
identified by the string CATALOGUE-ID in the catalogue directory, or a nothing
value with the id catalogue-detailer if that catalogue cannot be found.

If CATALOGUE-ID is #f, then we assume that no catalogues have been created
thus far, as no current-catalogue link exists.

CATALOGUE-ID:  string naming a catalogue as installed in a catalogue
               directory.
"
  (define (local-enter? path stat journey)
    (or (ok-depth? journey)
        (throw 'invalid-catalogue-store path)))
  (define (local-down path stat journey)
    "Upon descending in dir we want to create the template for a new
catalogue: we cons (name '()) to the front of journey."
    (make-journey (go-deeper (get-depth journey))
                  (log-add-catalogue (cataloguename path)
                                     (get-log journey)
                                     (get-cat journey))
                  (get-cat journey)))

  (lambda (catalogue-dir)
    (if catalogue-id
        (match (catalogue-system-fold local-enter? local-down
                                      (make-bare-journey 1 catalogue-dir)
                                      (discipline-directory catalogue-dir
                                                            catalogue-id))
          ((or () ($ <nothing> 'unknown-catalogue))
           (nothing 'catalogue-detailer (cons catalogue-id catalogue-dir)))
          ((catalogue) catalogue)
          (strange (throw 'glean-logic-error 'CATALOGUE-DETAILER strange)))
        ;; current-catalogue does not yet exist.
        ;; -> we assume no catalogues yet exist, so we start at 0.
        (make-bare-catalogue "catalogue-0"
                             (catalogue-directory catalogue-dir
                                                  "catalogue-0")))))


;;;; Install Discipline
;;;
;;; To install a new discipline we must install the discipline files in the
;;; store, and then instantiate a new catalogue, superseding the
;;; current-catalogue with one that encompasses it and a pointer to the newly
;;; installed discipline.  This procedure only handles installing the
;;; discipline in the store.

(define* (discipline-installer store-dir source-dir target-file)
  "Return a procedure of one argument, which when applied, installs the
discipline SOURCE-DIR in the store STORE-DIR, after which it activates a new
catalogue augmenting the current catalogue with a new catalogue stored at the
procedure's argument, which contains a pointer to the newly installed
discipline.

TARGET-FILE is the name under which we'll install the discipline in
STORE-DIR.

STORE-DIR:   string pointing to a store dir into which we will install.
SOURCE-DIR:  string pointing to the directory containing a discipline to be
             installed.
TARGET-FILE: string pointing to the name under which we should install the
             discipline to be installed in the store.
"
  (let ((name-in-store (relative-filename store-dir target-file)))

    (define (write-discipline)
      "Copy the discipline located at SOURCE-DIR into the store at STORE-DIR."
      ;; XXX: Should we be doing this in Scheme?
      (if (zero? (system* "cp" "-r" source-dir name-in-store))
          name-in-store
          (nothing 'discipline-installer "Error copying file.")))

    (lambda (catalogue-dir)
      (if (and target-file (file-exists? name-in-store))
          ;; We have a duplicate hash in the read-only store.  We won't install.
          (nothing 'discipline-installer `(duplicate . ,name-in-store))
          ;; XXX: We need to check source-dir in all imaginable ways to ensure
          ;; it is a real and safe discipline.
          (catch #t
            write-discipline
            (lambda (key . args)
              (nothing 'discipline-installer `(,key ,args))))))))

;;;; Install Catalogue

(define* (catalogue-installer catalogue #:key fake-dir)
  "Return a procedure of one argument, which when applied, installs a new
catalogue in the directory pointed to by its argument, based on CURR-CAT, and
returns this newly created catalogue or a nothing value with id
'catalogue-installer.

If FAKE-DIR is a string we will install CATALOGUE in FAKE-DIR instead. This is
used, for instance, for temporary catalogue creation.

CATALOGUE: <catalogue> object to be installed.
FAKE-DIR:  string pointing to an alternate directory into which the catalogue
           should be installed, or #f.
"
  (lambda (catalogue-dir)
    (let ((target-dir (discipline-directory (or fake-dir catalogue-dir)
                                            (get-catalogue-id catalogue))))
      (mkdir-p target-dir)
      (catch 'system-error
        (lambda ()
          (vlist-for-each (lambda (discipline)
                            (match discipline
                              ((name . store-file)
                               (symlink store-file (discipline-filename
                                                    target-dir name)))))
                          (get-disciplines catalogue))
          catalogue)                    ; Return the catalogue
        (lambda (key . args)
          (nothing 'catalogue-installer `(,key ,args)))))))


;;;; Catalogue Store Helpers
;;;
;;; These procedures derive and maintain catalogue state from the underlying
;;; filesystem.  At some point in the future results could be cached.

(define (current-catalogue-setter new-catalogue curr-cat-link)
  "Return a procedure of one argument, a string identifying a catalogue
directory, which when applied returns NEW-CATALOGUE after ensuring that the
filesystem symlink to `current-catalogue' identified by the string
CURR-CAT-LINK has been updated to point to NEW-CATALOGUE.  Return a nothing
value with id 'current-catalogue-setter if we run into trouble.

NEW-CATALOGUE: <catalogue> object which will be the new current catalogue.
CURR-CAT-LINK: string pointing to the current catalogue symlink.
"
  (lambda (catalogue-dir)
    (catch 'system-error
      (lambda ()
        (when (file-exists? curr-cat-link)
          (delete-file curr-cat-link))
        (symlink (catalogue-directory catalogue-dir
                                      (get-catalogue-id new-catalogue))
                 curr-cat-link)
        new-catalogue)                  ; Return new-catalogue.
      (lambda (key . args)
        (nothing 'current-catalogue-setter `(,key ,args))))))

(define (next-catalogue-counter-maker)
  "Return a procedure of one argument, a string identifying a catalogue
directory, which when applied, returns a counter which can be used to name the
next catalogue.  If we run into problems we will return a nothing value with
id 'next-catalogue-counter-maker."
  (define re (make-regexp "^catalogue-([0-9]+)$"))
  (define (local-enter? path stat journey) ; only descend root dir.
    (ok-depth? journey))
  (define (local-down path stat journey)
    "Upon descending we simply want to increase depth: we've descended root
dir, now we can locate the highest counter."
    (set-depth journey (go-deeper (get-depth journey))))
  (define (local-leaf path stat journey)
    "This should only occur for the symlink current-catalogue.  We are not
interested in it, so simply return JOURNEY."
    journey)
  (define (local-skip path stat journey)
    "If PATH's basename contains a counter, investigate it and store it in
JOURNEY's state if it is larger than the current state; else simply return
JOURNEY as is."
    (match (regexp-exec re (basename path))
      (#f journey)
      ((? regexp-match? candidate)
       (let ((counter (string->number (match:substring candidate 1))))
         (if (< (get-log journey) counter)
             (set-log journey counter)
             journey)))))

  (lambda (catalogue-dir)
    (match (catalogue-system-fold local-enter? local-down
                                  (make-bare-journey 3 ; Allow exactly 1 descent.
                                                     catalogue-dir
                                                     #:state 0)
                                  catalogue-dir
                                  #:local-skip local-skip
                                  #:local-leaf local-leaf)
      ((? number? counter) (1+ counter))
      (otherwise (nothing 'next-catalogue-counter-maker `(,otherwise))))))

(define (current-catalogue-namer curr-cat-link)
  "Return a procedure of one argument, a string identifying a catalouge
directory, which when applied, returns the name of the catalogue current
pointed to by CURR-CAT-LINK, or #f if CURR-CAT-LINK does not yet exist.

CURR-CAT-LINK: string pointing to the current catalogue symlink.
"
  (lambda (catalogue-dir)
    (catch 'system-error
      (lambda () (basename (readlink curr-cat-link)))
      (lambda (key . args) #f))))      ; Current Catalogue does not yet exist.

(define (tmp-dir-fetcher)
  "Return a procedure of one argument, a string identifying a catalogue
directory, which when applied, returns a directory name to a temporary
directory on the filesystem."
  (lambda (catalogue-dir)
    (catch 'system-error
      (lambda ()
        (let ((dir (tmpnam)))
          (mkdir-p dir)
          dir))
      (lambda (key . args)
        ((tmp-dir-fetcher) catalogue-dir)))))


;;;; IO Catalogues Monad
;;;
;;; A specialized monad for handling UI & IO in relation to Catalogues.

(define (catalogue-return value)
  (lambda (catalogue-dir)
    value))

(define (catalogue-bind mvalue mproc)
  (lambda (catalogue-dir)
    (let ((value (mvalue catalogue-dir)))
      (if (nothing? value)
          value                         ; value is an error!
          ((mproc value) catalogue-dir)))))

(define-monad catalogue-monad
  (bind   catalogue-bind)
  (return catalogue-return))


;;;; Composite procedures
;;;
;;; These procedures use the catalogue monad to delay IO operations and handle
;;; nothing return values.

(define (mcatalogue-list catalogue-dir)
  "Analyze the directory identified by the string CATALOGUE-DIR and emit a
summary message for each catalogue encountered there.

CATALOGUE-DIR: string pointing to the catalogue directory.
"
  ((mlet* catalogue-monad
       ((catalogues (catalogue-lister)))
     (return catalogues))
   catalogue-dir))

(define (mcatalogue-show catalogue-dir catalogue-id)
  "Analyze the directory identified by the string CATALOGUE-DIR and emit an
overview of the catalogue identified by the string CATALOGUE-ID.

CATALOGUE-DIR: string pointing to the catalogue directory.
CATALOGUE-ID:  string naming the catalogue as installed in CATALOGUE-DIR.
"
  ((mlet* catalogue-monad
       ((catalogue (catalogue-detailer catalogue-id)))
     (return catalogue))
   catalogue-dir))

(define (mcatalogue-install catalogue-dir curr-cat-link store-dir source-dir)
  "Generate a procedure to install the discipline SOURCE-DIR in the store at
STORE-DIR, and activate the newly created catalogue at CATALOGUE-DIR.

CATALOGUE-DIR: string pointing to the catalogue directory.
CURR-CAT-LINK: string pointing to the current catalogue symlink.
STORE-DIR:     string pointing to a store dir into which we will install the
               discipline.
SOURCE-DIR:    string pointing to the directory containing a discipline to be
               installed.
"
  ((mlet* catalogue-monad
       ((name    -> (basename source-dir))
        (tmp-cat -> (mcatalogue-tmp catalogue-dir curr-cat-link source-dir))
        (tmp-lib -> (catalogue-hash (relative-filename
                                     (get-catalogue-dir tmp-cat)
                                     (get-catalogue-id  tmp-cat))))
        (target  -> (store-name tmp-lib (string->symbol
                                         (basename source-dir))))
        (store-path (discipline-installer store-dir source-dir target))
        (cat-name   (current-catalogue-namer curr-cat-link))
        (curr-cat   (catalogue-detailer cat-name))
        (counter    (next-catalogue-counter-maker))
        (new-cat -> (augment-catalogue curr-cat counter name store-path))
        (new-curr   (catalogue-installer new-cat))
        (catalogue  (current-catalogue-setter new-curr curr-cat-link)))
     (return catalogue))
   catalogue-dir))

;;; FIXME: we should install mechanisms for deleting the temporary files again.
;;; Prbly something along the lines of inspecting tmp-cat, to find its dir,
;;; then deleting all temporary directories referenced in by symlinks in that
;;; dir (we should find a reliable way of distinguishing tmp from normal
;;; dirs!), after which we can delete tmp-cat's dir itself.
(define (mcatalogue-tmp catalogue-dir curr-cat-link source-dir)
  "Generate a procedure to install the discipline at SOURCE-DIR in a temporary
store.  This tmp-store can be used by procedures that need to perform
set/discipline introspection, without having the discipline we are installing
in the store proper.

CATALOGUE-DIR: string pointing to the catalogue directory.
CURR-CAT-LINK: string pointing to the current catalogue symlink.
SOURCE-DIR:    string pointing to the directory containing a discipline to be
               installed.
"
  ((mlet* catalogue-monad
       ((name      ->  (basename source-dir))
        (tmp-store-dir (tmp-dir-fetcher))
        (store-path    (discipline-installer tmp-store-dir source-dir name))
        (cat-name      (current-catalogue-namer curr-cat-link))
        (curr-cat      (catalogue-detailer cat-name))
        (tmp-cat-dir   (tmp-dir-fetcher))
        (new-cat   ->  (augment-catalogue curr-cat 0 name store-path
                                          tmp-cat-dir))
        (catalogue ->  ((catalogue-installer new-cat) tmp-cat-dir)))
     (return catalogue))
   catalogue-dir))

(define (mcatalogue-remove catalogue-dir curr-cat-link disc-id)
  "Create and activate a new revision of current catalogue, with the
discipline identified by DISC-ID removed.

CATALOGUE-DIR: string pointing to the catalogue directory.
CURR-CAT-LINK: string pointing to the current catalogue symlink.
DISC-ID:       string naming the discipline we want to remove through its
               #:set-id.
"
  ((mlet* catalogue-monad
       ((name       (current-catalogue-namer curr-cat-link))
        (curr-cat   (catalogue-detailer name))
        (counter    (next-catalogue-counter-maker))
        (new-cat -> (impair-catalogue curr-cat counter disc-id))
        (new-curr   (catalogue-installer new-cat))
        (catalogue  (current-catalogue-setter new-curr curr-cat-link)))
     (return catalogue))
   catalogue-dir))

;;; catalogues ends here
