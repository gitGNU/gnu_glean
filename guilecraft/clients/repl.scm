;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

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
;;; Provide a simple guilecraft client which can be run from a guile
;;; prompt.
;;;
;;;; Code:

(define-module (guilecraft clients repl)
  #:use-module (rnrs)
  #:use-module (guilecraft clients monadic-min)
  #:use-module (guilecraft monads)
  #:use-module (guilecraft config)
  #:use-module (guilecraft data-types sets)
  #:export (
            repl-client
            register
            login
            delete
            available
            detail
            activate
            next
            solve
            ))

(define (repl-client)
  (display 'error))

(define id #f)                  ; Session data, mutable!
(define data #f)                ; Last response, mutable!

(define* (register name #:optional lounge library)
  (let* ((lng (if lounge lounge %profile-socket-file%))
         (lib (if library library %module-socket-file%))
         (rsp (register-player name lng lib)))
    (if (state? rsp)
        (begin
          (set! id rsp)
          (set! data 'unimportant)
          (guide `("~a has been registered.\n" ,name))
          (suggest help-available help-activate))
        (begin
          (set! id #f)
          (set! data #f)
          (cond ((eqv? 'name-taken (car rsp))
                 (guide `("'~a' is no longer available. "
                          ,(cadr rsp))
                        '("Please try a different user name, ")
                        `("or sign in as '~a'.\n" ,(cadr rsp)))
                 (suggest help-login help-register))
                ((eqv? 'invalid-username (car rsp))
                 (guide `("'~a' is not a valid user name. "
                          ,(cadr rsp))
                        '("Please try again.\n")
                        '("(Usernames should be strings)\n"))
                 (suggest help-login help-register))
                (else (inform rsp)))))))
(define* (login name #:optional lounge)
  (let* ((lng (if lounge lounge %profile-socket-file%))
         (rsp (authenticate-player name lng)))
    (if (state? rsp)
        (begin
          (set! id rsp)
          (set! data 'unimportant)
          (guide `("Welcome back ~a!\n" ,name)
                 '("You are now logged in."))
          (suggest help-next help-available help-activate))
        (begin
          (set! id #f)
          (set! data #f)
          (cond ((eqv? 'unknown-name (car rsp))
                 (guide `("'~a' is unknown at this lounge. "
                          ,(cadr rsp))
                        '("Please try again or register.\n")
                        '("(User names are case sensitive)\n"))
                 (suggest help-login help-register))
                ((eqv? 'invalid-username (car rsp))
                 (guide `("'~a' is not a valid user name. "
                          ,(cadr rsp))
                        '("Please try again.\n")
                        '("(Usernames should be strings)\n"))
                 (suggest help-login help-register))
                (else (inform rsp)))))))
(define (delete)
  (let ((rsp (delete-player id)))
    (if (stateful? rsp)
        (begin
          (set! id (state rsp))
          (set! data (car (result rsp)))
          (guide `("Your profile has now been deleted.\n"))
          (suggest help-login help-register))
        (begin
          (set! id #f)
          (set! data #f)
          (cond ((eqv? 'invalid-token (car rsp))
                 (guide `("Your session is no longer valid. ")
                        '("Please sign in again.\n"))
                 (suggest help-login help-register))
                (else (inform rsp)))))))
(define (available)
  (let ((rsp (known-modules id)))
    (if (stateful? rsp)
        (begin
          (set! id (state rsp))
          (set! data (result rsp))
          (for-each
           (lambda (module)
             (guide `("~a: ~a (~a)\n  ~a\n" ,(car module) ,(cadr module)
                      ,(caddr module) ,(cadddr module))))
           (result rsp))
          (suggest help-activate help-next help-detail))
        (begin
          (set! id #f)
          (set! data #f)
          (inform rsp)))))
(define (activate . ids)
  (let ((rsp (add-active-modules ids id)))
    (if (stateful? rsp)
        (begin
          (set! id (state rsp))
          (set! data (result rsp))
          (guide `("~a activated."
                   ,(string-join (map symbol->string ids) ", ")))
          (suggest help-next help-available))
        (begin
          (set! id #f)
          (set! data #f)
          (cond ((eqv? 'invalid-token (car rsp))
                 (guide `("Your session is no longer valid. ")
                        '("Please sign in again.\n"))
                 (suggest help-login help-register))
                (else (inform rsp)))))))
(define (next)
  (let ((rsp (next-challenge id)))
    (cond ((stateful? rsp)
           (set! id (state rsp))
           (set! data (result rsp))
           (guide `("New Challenge: ~a\n"
                    ,(q-text (car (result rsp)))))
           (suggest help-solve help-available))
          (else
           (set! id #f)
           (set! data #f)
           (cond ((eqv? 'invalid-token (car rsp))
                  (guide `("Your session is no longer valid. ")
                         '("Please sign in again.\n"))
                  (suggest help-login help-register))
                 (else (inform rsp)))))))
(define (solve answer)
  (let ((rsp (submit-answer answer id)))
    (cond ((stateful? rsp)
           (set! id (state rsp))
           (set! data (result rsp))
           (if (car (result rsp))
               (guide `("Correct! The answer is indeed '~a.'\n\n"
                        ,(s-text (cadr (result rsp)))))
               (guide `("Incorrect: the solution is:\n  '~a'\n\n"
                        ,(s-text (cadr (result rsp))))))
           (next))
          (else
           (set! id #f)
           (set! data #f)
           (cond ((eqv? 'invalid-token (car rsp))
                  (guide `("Your session is no longer valid. ")
                         '("Please sign in again.\n"))
                  (suggest help-login help-register))
                 (else (inform rsp)))))))

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
           ,%guilecraft-bug-report-address%)))
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
