;; repl-client-core.scm --- a guile repl client    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
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
;; Play Glean from Guile or Geiser (in Emacs).  Currently, to use, start the
;; client as usual, then connect to the default Guile REPL server, and enter
;; this module.
;;
;;; Code:

(define-module (glean client components repl-client-core)
  #:use-module (glean config)
  #:use-module (glean client monadic-min)
  #:use-module (glean common components)
  #:use-module (glean common monads)
  #:use-module (glean common utils)
  #:use-module (ice-9 match)
  #:use-module (rnrs)
  #:export (component))

;;;;; Preliminaries
;;;
;;; Really, when launched from the command line, using the usual launch
;;; sequence for the client, this should fire up a REPL at the shell prompt.
;;; Instead, currently, the user is forced to connect to the spawned server
;;; and 'enter' this module.  This renders the client launch sequence rather
;;; useless.
;;;
;;; I have not gotten around to figuring out how to do this yet…

(define (repl-client)
  ((@ (system repl server) spawn-server))
  (let main-loop ((duration (* 60 60 24)))
    (sleep duration)
    main-loop))

(define id #f)                  ; Session data, mutable!
(define data #f)                ; Last response, mutable!
(define mods-assoc #f)          ; assoc of set-ids -> hashes, mutable!


;;;;; Porcelain

(define (help)
  "Display a startup message and suggest first steps."
  (for-each guide
            '(("Welcome to Glean!\n\n")
              ("To start playing, first register an account or login.\n")))
  (suggest help-register help-login help-available help-detail))

(define start help)

(define* (register name password #:optional lounge library)
  "Register a player as NAME PASSWORD with LOUNGE, using LIBRARY (default to
the %default% values for both if they are not provided).  Then suggest next
actions."
  (let* ((lng (if lounge lounge %default-lounge%))
         (lib (if library library %default-library%))
         (rsp (register-player name password lng lib)))
    (cond ((state? rsp)
           (set! id rsp)
           (set! data 'unimportant)
           (guide `("~a has been registered.\n" ,name))
           (suggest help-available help-activate))
          (else (nothing-handler rsp)))))

(define* (login name password #:optional lounge)
  "Login the user NAME/PASSOWRD with LOUNGE (or %default-lounge% if not
provided).  Then provide suggested next steps."
  (let* ((lng (if lounge lounge %default-lounge%))
         (rsp (authenticate-player name password lng)))
    (cond ((state? rsp)
           (set! id rsp)
           (set! data 'unimportant)
           (guide `("Welcome back ~a!\n" ,name)
                  '("You are now logged in."))
           (suggest help-next help-available help-activate))
          (else (nothing-handler rsp)))))

(define (delete)
  "Delete the currently logged in user from lounge.  Then provide suggested
next steps."
  (let ((rsp (delete-player id)))
    (cond ((stateful? rsp)
           (set! id (state rsp))
           (set! data (car (result rsp)))
           (guide `("Your profile has now been deleted.\n"))
           (suggest help-login help-register))
          (else (nothing-handler rsp)))))

(define (available)
  "Return the available modules from the library server.  Then provide
suggested next steps."
  (let ((rsp (known-modules (if id id dummy-state))))
    (cond ((stateful? rsp)
           (set! id (state rsp))
           (set! data (result rsp))
           (set! mods-assoc
                 (map
                  (lambda (module)
                    ;; id, name, version, synopsis
                    (guide `("~a: ~a (~a)\n  ~a\n" ,(cadr module)
                             ,(caddr module) ,(cadddr module)
                             ,(cadddr (cdr module))))
                    ;; assoc list entry: (id . hash)
                    (cons (cadr module) (car module)))
                  (result rsp)))
           (if (dummy-state? id)
               (suggest help-register help-login help-available help-detail)
               (suggest help-activate help-next help-detail)))
          (else (nothing-handler rsp)))))

(define (detail set-id)
  "Provide a detailed view of the set identified by SET-ID.  Then provide
suggested next steps."
  (let ((hash (assv-ref mods-assoc set-id)))
    (if (not hash)
        (begin
          (guide `("'~a' was not found amongst our modules.\n" ,set-id))
          (guide `("Please try again or Execute (available) first.\n"))
          (if (dummy-state? id)
              (suggest help-register help-login help-available help-detail)
              (suggest help-activate help-next help-detail)))
        (let ((rsp (view-set hash (if id id dummy-state))))
          (cond ((stateful? rsp)
                 (set! id (state rsp))
                 (set! data (result rsp))
                 (match (result rsp)
                   ((hash id name vers keyw syn desc auth res attr prop cont
                          logo)
                    (format #t "Name (version): ~a (~a)\n" name vers)
                    (format #t "Keywords: ~a\n" keyw)
                    (format #t "Synopsis: ~a\n" syn)
                    (format #t "Description: ~a\n" desc)
                    (format #t "Author: ~a\n" auth))
                   (_ (error "DETAIL -- Unexpected server response:"
                             (result rsp))))
                 (if (dummy-state? id)
                     (suggest help-register help-login help-available
                              help-detail)
                     (suggest help-activate help-next help-detail)))
                (else (nothing-handler rsp)))))))

(define (activate . ids)
  "Activate the disciplines identified by IDS for the currently logged in
user.  Then provide suggested next steps."
  (let* ((hashes (map (lambda (id)
                        (assv-ref mods-assoc id))
                      ids))
         (rsp    (add-active-modules hashes id)))
    (cond ((stateful? rsp)
           (begin
             (set! id (state rsp))
             (set! data (result rsp))
             (guide `("~a activated."
                      ,(string-join (map symbol->string ids) ", ")))
             (suggest help-next help-available)))
          (else (nothing-handler rsp)))))

(define (deactivate . ids)
  "Deactivate the disciplines identified by IDS for the currently logged in
user.  Then provide suggested next steps."
  (let* ((hashes (map (lambda (id)
                        (assv-ref mods-assoc id))
                      ids))
         (rsp    (add-active-modules hashes id 'negate)))
    (cond ((stateful? rsp)
           (begin
             (set! id (state rsp))
             (set! data (result rsp))
             (guide `("~a deactivated."
                      ,(string-join (map symbol->string ids) ", ")))
             (suggest help-next help-available)))
          (else (nothing-handler rsp)))))

(define (next)
  "Provide the next challenge for the currently logged in user.  Then provide
suggested next steps."
  (let ((rsp (next-challenge id)))
    (cond ((stateful? rsp)
           (set! id (state rsp))
           (set! data (result rsp))
           (match (car data)
             (((? pair? (q-text . q-media)) (? list? options)
               (? symbol? type))
              (guide `("~a\n" ,q-text))
              (cond ((eqv? type 'info)
                     ;; no action needed?
                     )
                    ((eqv? type 'open)
                     ;; no action needed?
                     )
                    ((eqv? type 'single)
                     (for-each (lambda (option counter)
                                 (guide `("~a: ~a\n" ,counter ,(car option))))
                               options (seq 1 (length options))))
                    ((eqv? type 'multi)
                     (for-each (lambda (option counter)
                                 (guide `("~a: ~a\n" ,counter ,(car option))))
                               options (seq 1 (length options)))))))
           (suggest help-solve help-available))
          (else (nothing-handler rsp)))))

(define* (solve #:optional (answer ""))
  "Submit a solution or request to move forward for the currently logged in
user.  Then provide suggested next steps."
  (let ((rsp (submit-answer answer id)))
    (cond ((stateful? rsp)
           (set! id (state rsp))
           (set! data (result rsp))
           (match (result rsp)
             ((#t "irrelevant")
              ; nothing to do
              )
             ((#t solution)
              (guide `("Correct! The answer is indeed '~a'.\n\n" ,solution)))
             ((#f solution)
              (guide `("Incorrect: the solution is:\n  '~a'\n\n" ,solution)))
             (_ (error "SOLVE -- unexpected result" (result rsp))))
           (next))
          (else (nothing-handler rsp)))))

(define* (account)
  "Retrieve details about your account.  Then provide suggested next steps."
  (let ((rsp (view-player id)))
    (cond ((stateful? rsp)
           (set! id (state rsp))
           (set! data (result rsp))
           (match (result rsp)
             ((username lng-port lib-port active-modules)
              (guide `("Username: ~a\n" ,username)
                     `("Lounge Port: ~a\n" ,lng-port)
                     `("Library Port: ~a\n" ,lib-port)
                     `("Active Modules: ~a\n\n" ,active-modules)))
             (_ (guide `("Unexpected Response: ~a\n\n" ,(result rsp)))))
           (suggest help-next help-detail help-available))
          (else (nothing-handler rsp)))))


;;;;; Helpers

(define (nothing-handler rsp)
  (cond ((eqv? 'servers-down (nothing-id rsp))
         (guide `("The lounge and library seem to be down: ~a.\n"
                  ,(nothing-context rsp))
                '("Please check your connection or try again.\n")))
        ((eqv? 'lounge-down (nothing-id rsp))
         (guide `("The lounge at ~a is currently down.\n"
                  ,(nothing-context rsp))
                '("Please check your connection or try again.\n")))
        ((eqv? 'library-down (nothing-id rsp))
         (guide `("The library at ~a is currently down.\n"
                  ,(nothing-context rsp))
                '("Please check your connection or try again.\n")))
        ((eqv? 'name-taken (nothing-id rsp))
         (guide `("'~a' is no longer available. "
                  ,(nothing-context rsp))
                '("Please try a different user name, ")
                `("or sign in as '~a'.\n" ,(nothing-context rsp)))
         (suggest help-login help-register))
        ((eqv? 'invalid-username (nothing-id rsp))
         (guide `("'~a' is not a valid user name. "
                  ,(nothing-context rsp))
                '("Please try again.\n")
                '("(Usernames should be strings)\n"))
         (suggest help-login help-register))
        ((eqv? 'unknown-name (nothing-id rsp))
         (guide `("'~a' is unknown at this lounge. "
                  ,(nothing-context rsp))
                '("Please try again or register.\n")
                '("(User names are case sensitive)\n"))
         (suggest help-login help-register))
        ((eqv? 'invalid-token (nothing-id rsp))
         (guide `("Your session is no longer valid. ")
                '("Please sign in again.\n"))
         (suggest help-login help-register))
        ((eqv? 'exchange-error (nothing-id rsp))
         (guide '("We got a negative response from the server.\n")
                '("It is:\n"))
         (rprinter (nothing-context rsp)))
        (else (inform rsp))))

(define (guide . output)
  "Use format to print each argument in OUTPUT to
current-output-port.\n
Example: (guide `(\"hello ~a\" ,name))."
  (for-each (lambda (out) (apply format #t out))
            output))

(define (inform rsp)
  "Inform user of unexpected result in the form of RSP.\n
Example: (inform rsp)."
  (guide `("I got an unexpected result: ~a.\n" ,rsp)
         `("Please report this issue to ~a.\n"
           ,%glean-bug-report-address%)))

(define (suggest . suggestions)
  "Pretty print suggested next actions.\n
Example: (suggest `(\"next\" . \"Retrieve the next challenge.\")
                  `(\"enable ids\" . \"Enable modules IDS.\"))."
  (guide '("\nNext steps…\n"))
  (apply guide (map (lambda (suggestion)
                      (list (string-append "  (" (car suggestion)
                                           ")\t— " (cdr suggestion)
                                           "\n")))
                    suggestions)))


;;;;; Messages

(define help-register
  (cons "register name password [lounge library]"
        "Register a new account as NAME, authenticated by PASSWORD."))
(define help-login
  (cons "login name password"
        "Start playing as NAME, authenticated by PASSWORD."))
(define help-available
  (cons "available"
        "Retrieve a list of available modules."))
(define help-detail
  (cons "detail id"
        "Retrieve detailed information of modules ID."))
(define help-activate
  (cons "activate id1 id2…"
        "Activate modules ID1, ID2, etc."))
(define help-next
  (cons "next"
        "Pose the next challenge."))
(define help-solve
  (cons "solve [\"your answer\"]"
        "Solve your current challenge."))

(define dummy-state (mk-state 1111 %default-lounge% %default-library%))

(define (dummy-state? id)
  (equal? dummy-state id))


;;;;; Finally, The Component

(define component
  (define-component
    #:name        "repl-client"
    #:provides    repl-client
    #:directories '()
    #:uses        (list (primary-config '()))))

;;; repl-client-core.scm ends here
