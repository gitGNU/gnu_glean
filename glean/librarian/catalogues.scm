;; catalogues.scm --- low-level catalogue manipulation   -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 18 November 2014
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
;; additionally used to always point to the catalogue currently in use.
;;
;; Catalogues are the data-stores actually used by the library at any given
;; point.  That is, the library normally never has a complete view of all the
;; disciplines in the store: only of those in the `current-catalogue.'
;;
;; Procedures interacting with the file-system itself, i.e. IO procedures, are
;; named actively (e.g. catalogue-install -> catalogue-installer).  These
;; procedures can be used in an IO monad passing around the
;; catalogue-directory, which will be implemented in the future.
;;
;;; Code:

(define-module (glean librarian catalogues)
  #:use-module (glean common config-utils)
  #:use-module (glean common monads)
  #:use-module (glean common utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-9 gnu)
  #:export     (catalogue-list
                catalogue-remove
                catalogue-show
                catalogue-install))


;;;; Catalogue Record Type Definition

(define-immutable-record-type catalogue
  (make-catalogue id disciplines)
  catalogue?
  (id          get-catalogue-id)
  (disciplines get-disciplines set-disciplines))

;;; Records used in Catalogue related file system traversal.

(define-immutable-record-type journey
  (make-journey depth-gauge journey-log)
  journey?
  (depth-gauge get-depth set-depth)
  (journey-log get-log   set-log))


;;;; UI
;;;
;;; High level UI procedures.  These should be the only emitting output.

(define (catalogue-install cat-dir curr-cat lib-dir target)
  "Attempt to install TARGET in the store at LIB-DIR, create a new catalogue
in CAT-DIR and update CURR-CAT.  We will exit with an error if we encounter a
problem."
  (match (mcatalogue-install cat-dir curr-cat lib-dir target)
    ((? nothing? nothing)
     (emit-error (nothing-id nothing) (nothing-context nothing)))
    ((? catalogue? catalogue)
     (emit-catalogue catalogue))))

(define (catalogue-remove cat-dir curr-cat disc-id)
  "Attempt to create and activate a new catalogue with the discipline
identified by DISC-ID removed from it.  We will exit with an error if we
encounter a problem."
  (match (mcatalogue-remove cat-dir curr-cat disc-id)
    ((? nothing? nothing)
     (emit-error (nothing-id nothing) (nothing-context nothing)))
    ((? catalogue? catalogue)
     (emit-catalogue catalogue))))

(define (catalogue-show cat-dir cat-id)
  "Attempt to show details about catalogue CAT-ID stored in CAT-DIR.  Exit
with an error if we encounter a problem."
  (match (mcatalogue-show cat-dir cat-id)
    ((? nothing? nothing)
     (emit-error (nothing-id nothing) (nothing-context nothing)))
    (((? catalogue? catalogue))
     (emit-catalogue catalogue))))

(define (catalogue-list cat-dir)
  "Attempt to list all catalogues stored in CAT-DIR.  Exit with an error if we
encounter a problem."
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
                     (leave (_ "Catalogue ~a could not be found in ~a.~%")
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
     (leave (_ "We encountered a problem installing the discipline.\n")))
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


;;;; Atomic Catalogue Operations

(define (make-bare-catalogue catalogue-id)
  "Return a fresh catalogue, with CATALOGUE-ID as the catalogue's id."
  (make-catalogue catalogue-id vlist-null))

(define (catalogue-add-discipline old-catalogue new-discipline)
  "Generate a new catalogue based on the catalogue OLD-CATALOGUE, which now
includes the discipline pair NEW-DISCIPLINE in it.  If NEW-DISCIPLINE's id was
already part of OLD-CATALOGUE, simply update that discipline's target to the
one provided by NEW-DISCIPLINE."
  (match old-catalogue
    (($ catalogue id disciplines)
     (match new-discipline
       ((name . target)
        (if (vhash-assoc name disciplines)
            (set-disciplines old-catalogue
                             (vhash-fold (lambda (fold-name fold-target result)
                                           (if (string=? fold-name name)
                                               (vhash-cons name target result)
                                               (vhash-cons fold-name
                                                           fold-target
                                                           result)))
                                         vlist-null
                                         disciplines))
            (set-disciplines old-catalogue
                             (vhash-cons name target disciplines))))))))

(define (catalogue-remove-discipline old-catalogue discipline-id)
  "Generate a new catalogue based on the catalogue OLD-CATALOGUE, which now
no longer containes the discipline identified by DISCIPLINE-ID."
  (match old-catalogue
    (($ catalogue id disciplines)
     (set-disciplines old-catalogue
                      (vhash-delete discipline-id disciplines)))))

(define (augment-catalogue old-catalogue counter new-discipline)
  "Return a new catalogue, incorporating the disciplines of OLD-CATALOGUE,
named with COUNTER, and augmented by NEW-DISCIPLINE.  OLD-CATALOGUE should be
a catalogue or '().  If the latter we build the resulting catalogue out of a
bare catalogue."
  (catalogue-add-discipline (if (null? old-catalogue)
                                (make-bare-catalogue
                                 (make-catalogue-name counter))
                                (make-catalogue
                                 (make-catalogue-name counter)
                                 (get-disciplines (car old-catalogue))))
                            (cons (basename new-discipline) new-discipline)))

(define (impair-catalogue old-catalogue counter old-id)
  "Return a new catalogue, incorporating all disciplines of OLD-CATALOGUE,
except for the one identified by OLD-ID.  This catalogue will be named using
COUNTER.  If OLD-CATALOGUE is '(), create a new, empty catalogue."
  (catalogue-remove-discipline (if (null? old-catalogue)
                                   (make-bare-catalogue
                                    (make-catalogue-name counter))
                                   (make-catalogue
                                    (make-catalogue-name counter)
                                    (get-disciplines (car old-catalogue))))
                               old-id))

;;;; Catalogue File System Semantics
;;;
;;; The Catalogue system maps to a filesystem structure.  This section maps
;;; that structure to predicates, accessors etc.

(define (relative-filename . components)
  "Return the string which consists of COMPONENTS, joined by this operating
system's filename separator."
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
symlink.  Return #t if so, #f otherwise."
  (eqv? (stat:type stat) 'symlink))

(define (catalogue-directory catalogue-dir catalogue-id)
  "Return a catalogue directory string; a string pointing towards the
catalogue named by CATALOGUE-ID in CATALOGUE-DIR."
  (relative-filename catalogue-dir catalogue-id))

(define (discipline-directory catalogue-dir catalogue-id)
  "Return a catalogue directory string which includes the standard glean
load-path-suffix for disciplines; a string pointing towards the catalogue
disciplines in the catalogue identified by CATALOGUE-ID in CATALOGUE-DIR.

In contrast to `catalogue-directory', this points to the folder, within a
specific catalogue, which contains disciplines.  `catalogue-directory' points
to the catalogue itself within the catalogue store."
  (relative-filename (catalogue-directory catalogue-dir catalogue-id)
                     (load-path-suffix)))

(define (discipline-filename catalogue-dir discipline-id)
  "Return a discipline file name: a string pointing towards the discipline
named by DISCIPLINE-ID in the catalogue identified by CATALOGUE-DIR."
  (relative-filename catalogue-dir discipline-id))

(define (make-catalogue-name counter)
  "Return a new catalogue-name of the form `catalogue-COUNTER'."
  (string-append "catalogue-" (number->string counter)))

(define (cataloguename path)
  "Return the string identifying the catalogue portion of PATH or raise an
error.  This procedure works similar to basename, except it does not return
the filename part of PATH, but the part that would identify the catalogue."
  (basename (string-drop-right path (string-length (load-path-suffix)))))

;;; The catalogue folder contains catalogues, which in turn contains the
;;; LOAD-PATH-SUFFIX, and finally symlinks to the disciplines in the store.
;;; As a result, any folder recursion starting at %catalogue-dir% should have
;;; a maximum depth of 3 (0 indexed).
;;;
;;; Despite the above reasoning, there are a number of hard-coded bits
;;; relating to depth & catalogue path which displease me.

(define (ok-depth? journey)
  "Return #t if the gauge in JOURNEY indicates an acceptable depth."
  (< (get-depth journey) 4))

(define (go-deeper depth)
  "Increase the depth of the gauge in JOURNEY."
  (1+ depth))

(define (come-up depth)
  "Decrease the depth of the gauge in JOURNEY."
  (1- depth))

;;;; Journey/Filesystem Traversal Operations

(define* (make-bare-journey depth #:key (state '()))
  "Return a fresh journey."
  (make-journey depth state))

(define (log-add-catalogue catalogue-id log)
  "Return JOURNEY-LOG augmented by a fresh catalogue for CATALOGUE-ID."
  (cons (make-bare-catalogue catalogue-id) log))

(define (log-add-discipline discipline-filename log)
  "Return LOG with the first catalogue in it augmented by DISCIPLINE."
  ;; XXX: We have an ugly readlink call here, which is direct IO.  As
  ;; file-system fold is IO anyway, and as it is always embedded in the
  ;; catalogue-monad, this is not a problem, but it does escape this module's
  ;; naming pattern.
  (match log
    ((incomplete-catalogue . rest)
     (cons (catalogue-add-discipline incomplete-catalogue
                                     (cons (basename discipline-filename)
                                           (readlink discipline-filename)))
           rest))))

(define (catalogue-leaf path stat journey)
  "Add the discipline to the skeleton and return journey, or throw an
error — if it is not a symlink we have an invalid Catalogues store."
  (if (discipline? stat)
      (set-log journey (log-add-discipline path
                                           (get-log journey)))
      (throw 'invalid-catalogue-store path)))

(define (catalogue-up path stat journey)
  "Simply return JOURNEY unchanged."
  (set-depth journey (come-up (get-depth journey))))

(define (catalogue-skip path stat journey)
  "Emit warning, as unexpected, but simply return JOURNEY."
  (warning (_ "We unexpectedly skipped a directory: ~a") path)
  journey)

(define (catalogue-error path stat errno journey)
  "Unless ERRNO is 2, which would mean that we cannot find the precise file we
are looking for, emit a warning and proceed by returning JOURNEY.  In the
former case, raise an error."
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
record; DIR would normally be the default catalogue-dir."
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
directory, or null."
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
                                         (get-log journey)))))
  
  (lambda (catalogue-dir)
    (match (catalogue-system-fold local-enter? local-down
                                  (make-bare-journey 0) catalogue-dir)
      (() (nothing 'catalogue-lister (cons catalogue-dir #f)))
      (otherwise otherwise))))

;;;;; Catalogue Detail

(define (catalogue-detailer catalogue-id)
  "Return a procedure of one argument, a string identifying a catalogue
directory, which when applied, returns a list containing the catalogue record
identified by the string CATALOGUE-ID in the catalogue directory, or a nothing
value with the id catalogue-detailer if that catalogue cannot be found or if
CATALOGUE-ID is #f."
  (define (local-enter? path stat journey)
    (or (ok-depth? journey)
        (throw 'invalid-catalogue-store path)))
  (define (local-down path stat journey)
    "Upon descending in dir we want to create the template for a new
catalogue: we cons (name '()) to the front of journey."
    (make-journey (go-deeper (get-depth journey))
                  (log-add-catalogue (cataloguename path) (get-log journey))))

  (lambda (catalogue-dir)
    (match (catalogue-system-fold local-enter? local-down
                                  (make-bare-journey 1)
                                  (discipline-directory catalogue-dir
                                                        catalogue-id))
      (() (nothing 'catalogue-detailer (cons catalogue-id catalogue-dir)))
      (otherwise otherwise))))


;;;; Install Discipline
;;;
;;; To install a new discipline we must install the discipline files in the
;;; store, and then instantiate a new catalogue, superseding the
;;; current-catalogue with one that encompasses it and a pointer to the newly
;;; installed discipline.

(define (discipline-installer store-dir source-dir)
  "Return a procedure of one argument, which when applied, installs the
discipline SOURCE-DIR in the store STORE-DIR, after which it activates a new
catalogue augmenting the current catalogue with a new catalogue stored at the
procedure's argument, which contains a pointer to the newly installed
discipline."
  (define (write-discipline store-dir source-dir)
    "Copy the discipline located at SOURCE-DIR into the store at STORE-DIR."
    ;; XXX: Should we be doing this in Scheme?
    (system* "cp" "-r" source-dir store-dir))

  (lambda (catalogue-dir)
    ;; XXX: We need to check source-dir in all imaginable ways to ensure it is
    ;; a real and safe discipline.
    (catch #t
      (lambda ()
        (write-discipline store-dir source-dir)
        (relative-filename store-dir (basename source-dir)))
      (lambda (key . args)
        (nothing 'discipline-installer `(,key ,args))))))

;;;; Install Catalogue
;;;
;;; Catalogue Bearer will be used by utilities that perform store
;;; manipulations (after each-store manipulation it is likely that a new
;;; catalogue must be created).  It returns a procedure, so hopefully we can
;;; mitigate impurity this way.  The actual ugly IO is performed in
;;; write-catalogue, so as long as that is delayed, we could simply use
;;; bearer directly.

(define (catalogue-installer catalogue)
  "Return a procedure of one argument, which when applied, installs a new
catalogue in the directory pointed to by its argument, based on CURR-CAT, and
returns this newly created catalogue or a nothing value with id
'catalogue-installer."
  (lambda (catalogue-dir)
    (let ((target-dir (discipline-directory catalogue-dir
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

(define (current-catalogue-setter new-catalogue current-catalogue-link)
  "Return a procedure of one argument, a string identifying a catalogue
directory, which when applied returns NEW-CATALOGUE after ensuring that the
filesystem symlink to `current-catalogue' identified by the string
CURRENT-CATALOGUE-LINK has been updated to point to NEW-CATALOGUE.  Return a
nothing value with id 'current-catalogue-setter if we run into trouble."
  (lambda (catalogue-dir)
    (catch 'system-error
      (lambda ()
        (when (file-exists? current-catalogue-link)
          (delete-file current-catalogue-link))
        (symlink (catalogue-directory catalogue-dir
                                      (get-catalogue-id new-catalogue))
                 current-catalogue-link)
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
                                                     #:state 0)
                                  catalogue-dir
                                  #:local-skip local-skip
                                  #:local-leaf local-leaf)
      ((? number? counter) (1+ counter))
      (otherwise (nothing 'next-catalogue-counter-maker `(,otherwise))))))

(define (current-catalogue-namer curr-cat-link)
  "Return a procedure of one argument, a string identifying a catalouge
directory, which when applied, returns the name of the catalogue current
pointed to by CURR-CAT-LINK, or a nothing with id 'current-catalogue-namer."
  (lambda (catalogue-dir)
    (catch 'system-error
      (lambda () (basename (readlink curr-cat-link)))
      (lambda (key . args)
        (nothing 'current-catalogue-namer `(,key ,args))))))


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
summary message for each catalogue encountered there."
  ((mlet* catalogue-monad
       ((catalogues (catalogue-lister)))
     (return catalogues))
   catalogue-dir))

(define (mcatalogue-show catalogue-dir catalogue-id)
  "Analyze the directory identified by the string CATALOGUE-DIR and emit an
overview of the catalogue identified by the string CATALOGUE-ID."
  ((mlet* catalogue-monad
       ((catalogue (catalogue-detailer catalogue-id)))
     (return catalogue))
   catalogue-dir))

(define (mcatalogue-install catalogue-dir curr-cat-link store-dir source-dir)
  "Generate a procedure to install the discipline SOURCE-DIR in the store at
STORE-DIR, and activate the newly created catalogue at CATALOGUE-DIR."
  ((mlet* catalogue-monad
       ((store-path (discipline-installer store-dir source-dir))
        (counter    (next-catalogue-counter-maker))
        (name       (current-catalogue-namer curr-cat-link))
        (curr-cat   (catalogue-detailer name))
        (new-cat -> (augment-catalogue curr-cat counter store-path))
        (new-curr   (catalogue-installer new-cat))
        (catalogue  (current-catalogue-setter new-curr curr-cat-link)))
     (return catalogue))
   catalogue-dir))

(define (mcatalogue-remove catalogue-dir curr-cat-link disc-id)
  "Create and activate a new revision of current catalogue, with the
discipline identified by DISC-ID removed."
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
