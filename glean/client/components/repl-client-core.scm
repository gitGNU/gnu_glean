;;; glean --- fast learning tool.         -*- coding: utf-8 -*-

;;;; REPL Client

;; Copyright (C) 2008, 2010, 2012 Alex Sassmannshausen

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
;;; Provide a simple glean client which can be run from a guile
;;; prompt.
;;;
;;;; Code:

(define-module (glean client components repl-client-core)
  #:use-module (glean config)
  #:use-module (glean client monadic-min)
  #:use-module (glean common components)
  #:use-module (glean common monads)
  #:use-module (glean common utils)
  #:use-module (glean library sets)     ; Should be obsolete
  #:use-module (rnrs)
  #:export (component))

(define (repl-client)
  ((@ (system repl server) spawn-server))
  (let main-loop ((duration (* 60 60 24)))
    (sleep duration)
    main-loop))

(define id #f)                  ; Session data, mutable!
(define data #f)                ; Last response, mutable!
(define mods-assoc #f)          ; assoc of set-ids -> hashes, mutable!

(define* (register name password #:optional lounge library)
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
  (let ((rsp (delete-player id)))
    (cond ((stateful? rsp)
           (set! id (state rsp))
           (set! data (car (result rsp)))
           (guide `("Your profile has now been deleted.\n"))
           (suggest help-login help-register))
          (else (nothing-handler rsp)))))
(define (available)
  (let ((rsp (known-modules id)))
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
           (suggest help-activate help-next help-detail))
          (else (nothing-handler rsp)))))
(define (activate . ids)
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
(define (next)
  (let ((rsp (next-challenge id)))
    (cond ((stateful? rsp)
           (set! id (state rsp))
           (set! data (result rsp))
           (guide `("New Challenge: ~a\n"
                    ,(q-text (car (result rsp)))))
           (suggest help-solve help-available))
          (else (nothing-handler rsp)))))
(define (solve answer)
  (let ((rsp (submit-answer answer id)))
    (cond ((stateful? rsp)
           (set! id (state rsp))
           (set! data (result rsp))
           (if (car (result rsp))
               (guide `("Correct! The answer is indeed '~a'.\n\n"
                        ,(s-text (cadr (result rsp)))))
               (guide `("Incorrect: the solution is:\n  '~a'\n\n"
                        ,(s-text (cadr (result rsp))))))
           (next))
          (else (nothing-handler rsp)))))

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

(define help-register
  (cons "register name [lounge library]"
        "Register a new account as NAME."))
(define help-login
  (cons "login name"
        "Start playing as NAME."))
(define help-available
  (cons "available"
        "Retrieve a list of available modules."))
(define help-detail
  (cons "detail '(id1 id2…)"
        "Retrieve detailed information of modules ID1, ID2, etc."))
(define help-activate
  (cons "activate id1 id2…"
        "Activate modules ID1, ID2, etc."))
(define help-next
  (cons "next"
        "Pose the next challenge."))
(define help-solve
  (cons "solve \"your answer\""
        "Solve your current challenge."))

(define component
  (define-component
    #:name        "repl-client"
    #:provides    repl-client
    #:directories '()
    #:uses        (list (primary-config '()))))
