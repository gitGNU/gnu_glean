;; comtools.scm --- generic tools for IPC  -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen  <alex.sassmannshausen@gmail.com>
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
;; This module's aim is to provide high-level communication tools used by
;; base-server and monadic-min (i.e. UI and server parts).
;;
;; record->list* and list->record* are used to turn requests and responses
;; into tagged lists and back for the purpose of being pushed through the
;; sockets, as records cannot be sent straight through.
;;
;; gwrite and gread are robust wrappers around write and read. They aim for
;; two things:
;; - 1) transmission of symbols by transforming them to strings and back;
;; - 2) catching of any errors that may occur during reading / writing.
;;
;;; Code:

(define-module (glean common comtools)
  #:use-module (glean common base-requests)
  #:use-module (glean common library-requests)
  #:use-module (glean common lounge-requests)
  #:use-module (glean common utils)
  #:use-module (ice-9 match)
  #:use-module (rnrs records inspection)
  #:use-module (rnrs records procedural)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs base)
  #:use-module (srfi srfi-26)
  #:export (
            alive?
            exchange
            record->list*
            list->record*
            gwrite
            gread
            ))


;;;; Record Type Database

;;; record-kv-pairs contains a list of records that are sometimes transmitted
;;; through the sockets. As records cannot be transmitted directly between
;;; scheme applications, we disassemble them using record->list* and
;;; re-assemble them using list->record*.  Re-assembly requires knowledge of
;;; the record constructor.  This is provided by the database.

(define (get-rc rec-name)
  "Syntactic sugar. Return REC-NAME's constructor or #f."
  (assv-ref record-kv-pairs rec-name))

(define known-rc? get-rc)

(define record-kv-pairs
  `((request    . ,request)
    (response   . ,response)

    (aliveq     . ,aliveq)
    (quitq      . ,quitq)
    (acks       . ,acks)
    (negs       . ,negs)
    (unks       . ,unks)

    ;; profile requests
    (echoq      . ,echoq)
    (echos      . ,echos)
    (regq       . ,regq)
    (viewq      . ,viewq)
    (views      . ,views)
    (set!q      . ,set!q)
    (set!s      . ,set!s)
    (delpq      . ,delpq)
    (authq      . ,authq)
    (auths      . ,auths)
    (chauthq    . ,chauthq)
    (chauths    . ,chauths)
    (evauthq    . ,evauthq)

    ;; module requests
    (challq     . ,challq)
    (challs     . ,challs)
    (evalq      . ,evalq)
    (evals      . ,evals)
    (knownq     . ,knownq)
    (knowns     . ,knowns)
    (detailq    . ,detailq)
    (details    . ,details)
    (hashmapq   . ,hashmapq)
    (hashmaps   . ,hashmaps)
    (sethashesq . ,sethashesq)
    (sethashess . ,sethashess)))


;;;; High Level Commands

;;; The following procedures are used in servers and/or clients.  See further
;;; below for support functions.

(define (exchange request socket-path)
  "Connect to server at SOCKET-PATH, send REQUEST and return the response from
the server or #f if the communication fails."
  (guard (err ((eqv? err 'alive?) #f)
              (err
               (begin (clog err)
                      (raise (list 'exchange err)))))
         (let ((s (server socket-path)))
           (gwrite (record->list* request) s)
           (let ((result (list->record* (gread s))))
             (close s)
             result))))

(define (alive? socket-path)
  "Return #t if a Glean server exists at SOCKET-PATH, otherwise #f.

SOCKET-PATH should identify the unix domain socket where we expect a server."
  (catch #t
    (lambda ()
      (and (access? socket-path W_OK)
           (exchange (request (aliveq)) socket-path)))
    (lambda ignored #f)))

;;; record->list* allow us to scan arbitrary objects for contained records.
;;; Any contained records are turned into encoded lists, so that they can be
;;; sent safely through sockets to another scheme app.
;;;
;;; list->record*, in the other scheme app, then turns encoded lists back into
;;; the records for use within it.

(define (record->list* object)
  "Return a list if OBJECT is a record, recursing through the generated list
to reduce further records into lists."
;; Recurse through a record, turning each contained record into a list to pump
;; it through sockets.  OBJECT could be a list, atom or record.
  (match object
    (() object)
    ((first . rest)
     (cons (record->list* first) (record->list* rest)))
    ((? record? obj)
     (record->list* (record->list obj)))
    (_ object)))

(define (list->record* object)
  "Return a record for every tagged list within OBJECT, and a list for
every normal list.

Return #f if object is not a list."

  (define (pair->record* obj)
    (match obj
      ;; A record in list form is a record-name + a list (possibly the empty
      ;; list), e.g. '(score-card).
      (((? symbol? first) . ())
       (list->record obj))
      (((? symbol? first) . (? pair? rest))
       (list->record (cons first (pair->record* rest))))
      
      ;; The rest of the cases simply need to be recursively scanned for
      ;; contained records: 3 further simple cases of recursion.
      (((? pair? first) . (? pair? rest))
       (cons (pair->record* first) (pair->record* rest)))
      (((? pair? first) . rest)
       (cons (pair->record* first) rest))
      ((first . (? pair? rest))
       (cons first (pair->record* rest)))

      ;; Exit conditions
      ((first . rest) obj)              ; Just return plain pairs/lists
      (_ #f)))                          ; Return #f if obj not pair.

  (pair->record* object))

(define (gread port)
  "Return a scheme object upon success or #f upon failure. Read from PORT,
catching read errors as necessary."
  (define (encoded-symbol? obj)
    (and (string? obj) (< 9 (string-length obj)) (string=? (string-take obj 9)
                                                           ":symbol: ")))
  (define (decode-symbol obj)
    "Return OBJ or a symbol, derived OBJ if OBJ is an encoded-symbol.

This procedure is used because the (read) procedure has difficulty reading
symbols plain symbols."
    (if (encoded-symbol? obj) (string->symbol (string-drop obj 9)) obj))

  (catch #t
    (lambda ()
      (match (read port)
        ((? string? in) (decode-symbol in))
        (in in)))
    (lambda (k . a) (clog "gread: Port closed prematurely: " k a) #f)))

(define (gwrite object port)
  "Return #t on success, #f on failure. write OBJECT to PORT, catching write
errors and turning OBJECT from symbol into string if necessary."

  (define (encode-symbol obj)
    "Return obj or a string whose first word is :symbol: and whose second word
is OBJ if OBJ is a symbol.

This procedure is used because the (read) procedure has difficulty reading
symbols plain symbols."
    (if (symbol? obj)
        (string-append ":symbol: " (symbol->string obj))
        obj))
  
  (catch #t
    (lambda ()
      (write (match object
               ((? symbol? obj) (encode-symbol obj))
               (obj obj))
             port)
      #t)
    (lambda (k . a) (clog "gwrite: Port closed prematurely: " k a) #f)))

;;;; Support Functions

;;; These functions are should only be used within this module.  They provide
;;; the high level procedures with required tools.

(define (server socket-path)
  "Return a socket after connecting it to the server at SOCKET-PATH. Raise an
error with the id 'alive? if we are unable to connect to the server."
  (let* ((socket  (socket PF_UNIX SOCK_STREAM 0))
         (path    socket-path)
         (address (make-socket-address AF_UNIX path)))
    (catch 'system-error
      (lambda () (connect socket address) socket)
      (lambda ignore (raise 'alive?)))))

(define (record->list record)
  "Return a list consing RECORD's field-values together, and prepend
with RECORD's record-type-descriptor."
  (define (get-field-values rtd)
    (define (get-accessors index collected)
      (match index
        (-1    collected)
        (index (get-accessors (1- index)
                              (cons (record-accessor rtd index) collected)))))
    (map (cut <> record)
         (get-accessors (1- (vector-length (record-type-field-names rtd)))
                        '())))

  (if (record? record)
      (let ((rtd (record-rtd record)))
        (cons (record-type-name rtd) (get-field-values rtd)))
      (assertion-violation 'record->list
                           "RECORD is not actually a record!" record)))

(define (list->record object)
  "Return a record, assembled from the pieces of the tagged list
OBJECT.

Return OBJECT if OBJECT is not a list."
  (match object
    (((? symbol? rec-name) . rec-vals)
     (match rec-name
       ((? known-rc? rn) (apply (get-rc rn) rec-vals))
       (_ object)))
    (((not symbol? rec-name) . rec-val)
     (assertion-violation 'list->record "RN is not a symol." object rec-name))
    (obj obj)))

;;; comtools.scm ends here
