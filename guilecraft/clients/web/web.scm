;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;;; Web Client

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
;;; Provide a web user interface. It can be published to other network
;;; participants through a reverse proxy setup with other web servers
;;; (e.g. Apache, Nginx).
;;;
;;;; Code:

(define-module (guilecraft clients web web)
  #:use-module (artanis artanis)
  #:use-module (guilecraft clients monadic-min)
  #:use-module (guilecraft clients web bootstrap)
  #:use-module (guilecraft config)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft monads)
  #:use-module (guilecraft utils)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:export (web-client))

;; Make text more easily fit the 72columns
(define (sa . args)
  (apply string-append args))

(define (web-client)
  (artanis-dispatch %guilecraft-dir%
                    #:index index
                    #:login (cons "/account" login)
                    #:register (cons "/account" register)
                    #:account (cons account "/login")
                    #:session session
                    #:detail detail
                    #:auth-action auth-action
                    #:reg-action reg-action
                    #:mod-action mod-action
                    #:del-action del-action
                    #:eval-action eval-action))

(define (index rc)
  (let* ((state (query-string->state rc))
         (width (if state "col-md-12" "col-md-8")))
    (frame #:state state
           #:rc    rc
           #:page `((div (@ (class "row"))
                         (div (@ (class ,width))
                              ,(panel %title% %intro%))
                         ,(if state
                              ""
                              `(div (@ (class "col-md-4"))
                                    ,(login-block))))
                    (div (@ (class "row"))
                         (div (@ (class ,width))
                              ,(list-available #f 'list)))))))

(define (login rc)
  (frame #:state (query-string->state rc)
         #:title (sa %title% " — Sign In")
         #:page  `((h1 "Sign In")
                   (p "Sign in to your account.")
                   ,(login-block))))
(define (register rc)
  (frame #:state (query-string->state rc)
         #:title (sa %title% " — Register")
         #:page  `((h1 "Account Registration")
                   (p "Sign up by completing this form.")
                   ,(form "/reg-action"
                          `((div (@ (class "form-group"))
                                 (label (@ (for "username")) "Username:")
                                 (input (@ (name        "username")
                                           (type        "text")
                                           (placeholder "Username")
                                           (class       "form-control"))))
                            (div (@ (class "form-group"))
                                 (label (@ (for "password")) "Password:")
                                 (input (@ (name        "password")
                                           (type        "password")
                                           (placeholder "Password")
                                           (class       "form-control"))))
                            (div (@ (class "form-group"))
                                 (label (@ (for "lounge"))
                                        "Address of the lounge:")
                                 (input (@ (name        "lounge")
                                           (type        "text")
                                           (value       "Automatic")
                                           (class       "form-control"))))
                            (div (@ (class "form-group"))
                                 (label (@ (for "library"))
                                        "Address of the library:")
                                 (input (@ (name        "library")
                                           (type        "text")
                                           (value       "Automatic")
                                           (class       "form-control"))))
                            (button (@ (type  "submit")
                                       (class "btn btn-success"))
                                    "Sign Up"))))))

(define (account rc)
  (let* ((st8     (query-string->state rc))
         (rsp     (view-player st8)))
    (if (stateful? rsp)
        (match (result rsp)
          ((username lng-port lib-port active-modules)
           (let ((st8 (state rsp)))
             (frame
              #:state st8
              #:rc    rc
              #:title (sa %title% " — My Account")
              #:page  `((h1 "Check Your Account Details")
                        ,(activity-form active-modules st8)
                        ,(identity-form username st8)
                        ,(server-form lng-port lib-port st8)
                        ,(expiration-form st8))))))
        (let ((rsp (fallback-view-player st8)))
          (if (stateful? rsp)
              (match (car (result rsp))
                ((username lng-port lib-port raw-active-modules)
                 (frame
                  #:state st8
                  #:rc    rc
                  #:title (sa %title% " — Fallback Account Management")
                  #:page  `((h1 "Check your Account Details")
                            ,(alert "The library server seems to be \
down. Perhaps your selected server is invalid?" 'danger)
                            ,(identity-form username st8)
                            ,(server-form lng-port lib-port st8)
                            ,(expiration-form st8)))))
              (frame #:page (nothing-handler rsp)))))))

(define (expiration-form st8)
  (form "/del-action"
        `((h2 "Expiration")
          (p "You can delete your account by clicking on the button \
the button below. This action is immediate and cannot be undone.")
          ,(state->form-fields st8)
          (button (@ (class "btn btn-danger")
                     (type  "submit"))
                  "Delete Account"))))

(define (server-form lng-port lib-port st8)
  (form "/mod-action"
        `((h2 "Servers")
          (p "This lounge is at: "  ,lng-port)
          (p "Please enter your desired new library port, or \
clear the field to set it back to the default for this space.")
          (div (@ (class "form-group"))
               (label (@ (for "value")) "Library Address:")
               (input (@ (name        "value")
                         (type        "text")
                         (value       ,lib-port)
                         (class       "form-control"))))
          ,(state->form-fields st8)
          (input (@ (type  "hidden")
                    (name  "operation")
                    (value "mod-server")))
          (button (@ (class "btn btn-primary")
                     (type  "submit"))
                  "Update Library"))))

(define (identity-form username st8)
  `(,(form "/mod-action"
           `((h2 "Identity — Username")
             (p "Please enter a new username and your current \
password to change your username.")
             (div (@ (class "form-group"))
                  (label (@ (for "username")) "Username:")
                  (input (@ (name        "username")
                            (type        "text")
                            (value       ,username)
                            (class       "form-control"))))
             (div (@ (class "form-group"))
                  (label (@ (for "password")) "Password:")
                  (input (@ (name        "password")
                            (type        "password")
                            (placeholder "Password")
                            (class       "form-control"))))
             ,(state->form-fields st8)
             (input (@ (type  "hidden")
                       (name  "operation")
                       (value "name")))
             (button (@ (class "btn btn-primary")
                        (type  "submit"))
                     "Update Username")))
    ,(form "/mod-action"
           `((h2 "Identity — Password")
             (p "Please enter a new password to change your current \
password.")
             (div (@ (class "form-group"))
                  (label (@ (for "value")) "New password:")
                  (input (@ (name        "value")
                            (type        "password")
                            (placeholder "New password")
                            (class       "form-control"))))
             ,(state->form-fields st8)
             (input (@ (type  "hidden")
                       (name  "operation")
                       (value "password")))
             (button (@ (class "btn btn-primary")
                        (type  "submit"))
                     "Update Password")))))

(define (activity-form active-modules st8)
  (define (list-active)
    (if (null? active-modules)
        ""
        (form  "/mod-action"
               `((p "You can disable any of your active modules by
checking their checkbox and clicking on ‘De-activate.‘")
                 ,(render-modules active-modules "Active Modules" st8
                                  "Disable" 'table)
                 ,(state->form-fields st8)
                 (input (@ (type  "hidden")
                           (name  "operation")
                           (value "deactivate-modules")))
                 (button (@ (class "btn btn-primary")
                            (type  "submit"))
                         "De-activate")))))
  `((h2 "Activity")
    ,(list-active)
    ,(form "/mod-action"
           `((p "You can enable new modules by checking their checkbox
and clicking on ‘Activate’.")
             ,(list-available "Enable" 'table st8)
             ,(state->form-fields st8)
             (input (@ (type  "hidden")
                       (name  "operation")
                       (value "activate-modules")))
             (button (@ (class "btn btn-primary")
                        (type  "submit"))
                     "Activate")))))

(define (detail rc)
  (let* ((st8 (query-string->state rc))
         (rsp (view-set (string->symbol (params rc "hash")) st8))
         (content (if (stateful? rsp)
                      (render-set (result rsp) (state rsp))
                      (nothing-handler rsp))))
    (frame
     #:state (if (stateful? rsp) (state rsp) #f)
     #:rc    rc
     #:title (sa %title% " — Set Details")
     #:page  `(,content))))

(define (session rc)
  (define (render-challenge challenge st8)
    (define (render-media media)
      `(,(map (lambda (img) (image-source img)) (media-images media))
        ,(map (lambda (vid) (video-source vid)) (media-videos media))
        ,(map (lambda (aud) `(audio (@ (src ,aud))))
              (media-audio media))
        ,(map (lambda (url) (website-source url)) (media-urls media))
        ,(map (lambda (book) `(p ,book))
              (media-books media))))
    (define (parse-q)
      (match challenge
        (((? pair? (q-text . q-media)) (? list? options)
          (? symbol? type))
         `((p ,(string-append "Question: " q-text))
           ,(cond ((eqv? type 'info)
                   `(input (@ (type        "hidden")
                              (name        "solution")
                              (value       "info"))))
                  ((eqv? type 'open)
                   `(input (@ (type        "text")
                              (placeholder "Enter your solution here…")
                              (name        "solution")
                              (class       "form-control")
                              (autofocus   "autofocus"))))
                  ((eqv? type 'single)
                   (map (lambda (option counter)
                          (let ((name (string-append "q-option-"
                                                     (number->string counter))))
                            `((label (@ (for ,name)
                                        (class "radio"))
                                     ,(car option)
                                     (input (@ (type  "radio")
                                               (name  "solution")
                                               (id    ,name)
                                               (value ,(car option)))))
                              ,(render-media (cdr option)))))
                        options (seq 1 (length options))))
                  ((eqv? type 'multi)
                   (map (lambda (option counter)
                          (let ((name (string-append "q-option-"
                                                     (number->string counter))))
                            `((label (@ (for ,name)
                                        (class "checkbox"))
                                     ,(car option)
                                     (input (@ (type  "checkbox")
                                               (name  ,(car option))
                                               (class "form-control")
                                               (id    ,name))))
                              ,(render-media (cdr option)))))
                        options (seq 1 (length options)))))
           (button (@ (class "btn btn-primary")
                      (type  "submit"))
                   "Solve")
           ,(render-media q-media)))))
    (panel "Challenge"
           `(,(form "/eval-action"
                    `(,(parse-q)
                      ,(state->form-fields st8))))))
  (let* ((st8 (query-string->state rc))
         (rsp (next-challenge st8)))
    (frame
     #:state (if (stateful? rsp) (state rsp) #f)
     #:rc    rc
     #:title (sa %title% " — Next Challenge…")
     #:page  (cond ((and (stateful? rsp)
                         (eqv? (car (result rsp))
                               'no-active-modules))
                    (alert "You have not yet enabled any modules. \
Please visit your account page where you will be able to enable some."
                           'info))
                   ((stateful? rsp)
                    (render-challenge (car (result rsp))
                                      (state rsp)))
                   (else
                    (nothing-handler rsp))))))
(define (auth-action rc)
  (let* ((name     (params rc "username"))
         (password (params rc "password"))
         ;;(params rc "lounge") (params rc "library")
         (st8      (authenticate-player name password %lounge-port%)))
    (post-auth st8 rc "aut-success")))
(define (reg-action rc)
  (let ((name     (params rc "username"))
        (password (params rc "password"))
        (lounge   (params rc "lounge"))
        (library  (params rc "library")))
    (let* ((lng   (if (string=? lounge "Automatic")
                      %lounge-port%
                      lounge))
           (lib   (if (string=? library "Automatic")
                      %library-port%
                      library))
           (st8   (register-player name password lng lib)))
      (post-auth st8 rc "reg-success"))))
(define (post-auth st8 rc msg)
  (if (state? st8)
      (redirect-to rc (wrap st8 "/account" (sa "result=" msg)))
      (response-emit
       (tpl->html
        (frame #:page (nothing-handler st8))))))
(define (mod-action rc)
  (define (parse-ids)
    (map (lambda (kv) (string->symbol (car kv)))
         (params-filter (lambda (kv)
                          (string=? (cadr kv) "on"))
                        rc)))
  (let ((st8 (query-string->state rc))
        (op  (params rc "operation")))
    (cond ((string=? op "activate-modules")
           (let* ((ids (parse-ids))
                  (rsp (add-active-modules ids st8)))
             (if (stateful? rsp)
                 (redirect-to rc (wrap (state rsp) "/session"
                                       "result=add-success"))
                 (response-emit
                  (tpl->html
                   (frame #:page (nothing-handler rsp)))))))
          ((string=? op "deactivate-modules")
           (let* ((ids (parse-ids))
                  (rsp (add-active-modules ids st8 'negate)))
             (if (stateful? rsp)
                 (redirect-to rc (wrap (state rsp) "/session"
                                       "result=rem-success"))
                 (response-emit
                  (tpl->html
                   (frame #:page (nothing-handler rsp)))))))
          ((string=? op "name")
           (let* ((value (cons (params rc "username")
                               (params rc "password")))
                  (rsp (modify-player (string->symbol op)
                                      value
                                      st8)))
             (if (stateful? rsp)
                 (redirect-to rc (wrap (state rsp) "/session"
                                       "result=mod-success"))
                 (response-emit
                  (tpl->html
                   (frame #:page (nothing-handler rsp)))))))
          ((string=? op "password")
           (let ((rsp (modify-player (string->symbol op)
                                     (params rc "value")
                                     st8)))
             (if (stateful? rsp)
                 (redirect-to rc (wrap (state rsp) "/session"
                                       "result=mod-success"))
                 (response-emit
                  (tpl->html
                   (frame #:page (nothing-handler rsp)))))))
          ((or (string=? op "prof-server")
               (string=? op "mod-server"))
           (let* ((value (if (string=? (params rc "value") "")
                             (if (string=? op "prof-server")
                                 %lounge-port%
                                 %library-port%)
                             (params rc "value")))
                  (rsp (modify-player (string->symbol op)
                                      value
                                      st8)))
             (if (stateful? rsp)
                 (redirect-to rc (wrap (state rsp) "/session"
                                       "result=mod-success"))
                 (response-emit
                  (tpl->html
                   (frame #:page (nothing-handler rsp)))))))
          (else
           (response-emit
            (tpl->html
             (frame #:page (nothing-handler (nothing 'invalid-form "")))))))))
(define (del-action rc)
  (let* ((st8 (query-string->state rc))
         (rsp (delete-player st8)))
    (if (stateful? rsp)
        (redirect-to rc "/?result=del-success")
        (response-emit
         (tpl->html
          (frame #:page (nothing-handler rsp)))))))
(define (eval-action rc)
  (let* ((st8    (query-string->state rc))
         (answer (let ((a (params rc "solution")))
                   (if a
                       (uri-decode a)
                       (map (lambda (kv)
                              (uri-decode (car kv)))
                            (params-filter (lambda (kv)
                                             (string=? (cadr kv) "on"))
                                           rc)))))
         (rsp    (submit-answer answer st8)))
    (cond ((and (stateful? rsp)
                (car (result rsp)))
           (redirect-to rc (wrap (state rsp) "/session"
                                 "result=eval-success")))
          ((and (stateful? rsp)
                (not (car (result rsp))))
           (response-emit
            (tpl->html
             (frame
              #:rc    rc
              #:state (state rsp)
              #:title (sa %title% " — Incorrect Answer")
              #:page  (panel "Solution"
                             `((p ,(sa "Incorrect. You answered: "
                                       (if (list? answer)
                                           (string-join answer ", ")
                                           answer)))
                               (p ,(sa "The solution is actually: "
                                       (if (list? answer)
                                           (string-join
                                            (map s-text
                                                 (cadr (result rsp)))
                                            ", ")
                                           (s-text (cadr (result rsp))))))
                               (p "Ready for your "
                                  (a (@ (href  ,(wrap (state rsp) "/session"))
                                        (title "Click for your next challenge"))
                                     "next challenge?"))))))))
          (else
           (response-emit
            (tpl->html
             (frame #:page (nothing-handler rsp))))))))

(define (next-block rc)
  (let* ((st8 (query-string->state rc))
         (rsp (next-challenge st8)))
    (cond ((stateful? rsp)
           `(div (@ (id "content"))
                 (p ,(q-text (car (result rsp))))))
          (else
           (nothing-handler rsp)))))
(define (login-block)
  (form "/auth-action"
        `((div (@ (class "form-group"))
               (label (@ (for "username"))
                      "Username:")
               (input (@ (type "text")
                         (placeholder "Username")
                         (class "form-control")
                         (name "username"))))
          (div (@ (class "form-group"))
               (label (@ (for "password"))
                      "Password:")
               (input (@ (type "password")
                         (placeholder "Password")
                         (class "form-control")
                         (name "password"))))
          (button (@ (type "submit")
                     (class "btn btn-success"))
                  "Play"))))

(define* (list-available #:optional (active #f) (format 'table)
                         (st8 (mk-state 37146
                                        %lounge-port%
                                        %library-port%)))
  (let ((rsp (known-modules st8)))
    (cond ((stateful? rsp)
           (render-modules (result rsp) "Available Modules"
                           st8 active format))
          (else
           (render-modules rsp "Available Modules"
                           st8 active format)))))

(define* (frame #:key (state #f)
                (page `(p "This is a test page"))
                (title (sa %title% " — Fast Learning Tool"))
                (rc #f))
  (define frame-helper (frame-maker %base-url%))
  (define (parse-help)
    (let ((msg (if rc (params rc "help") #f)))
      `(div (@ (class "row help"))
            (div (@ (class "container"))
                 (div (@ (class "col-md-4"))
                      (p ,(if msg
                              (cond ((string=? msg "register")
                                     "Complete this form to register.")
                                    (else "Unknown help message."))
                              "This is our contextual help. It's content \
will change depending on what action is being performe by the user.")))
                 (div (@ (class "col-md-4"))
                      (p "More Help:")
                      (ul (li "The Manual")
                          (li "The Module")))
                 (div (@ (class "col-md-4"))
                      (p "Final block."))))))
  (define (parse-msg)
    (let ((msg (if rc (params rc "result") #f)))
      (if msg
          (cond ((string=? msg "eval-success")
                 (alert "Correct! Onwards…" 'success))
                ((string=? msg "mod-success")
                 (alert "Your account has been updated." 'success))
                ((string=? msg "del-success")
                 (alert "Your account has been deleted." 'success))
                ((string=? msg "add-success")
                 (alert "Your selected modules have been activated."
                        'success))
                ((string=? msg "rem-success")
                 (alert "Your selected modules have been
de-activated." 'success))
                ((string=? msg "aut-success")
                 (alert "You are now logged in." 'success))
                ((string=? msg "reg-success")
                 (alert "Your account has been registered." 'success))
                (else
                 (alert "Success! But we don't know with what…"
                        'success)))
          "")))

  (frame-helper `(,(header-region (parse-help) state)
                  (div (@ (class "container")
                          (role  "main"))
                       ,(parse-msg)
                       ,page)
                  ,(footer-region))
                title))

(define* (header-region help #:optional st8)
  `(div (@ (class "navbar navbar-inverse") ; navbar-fixed-top
           (role  "navigation"))
        ,help
        (div (@ (class "container"))
             (div (@ (class "navbar-header"))
                  (button (@ (type        "button")
                             (class       "navbar-toggle")
                             (data-toggle "collapse")
                             (data-target ".navbar-collapse"))
                          (span (@ (class "sr-only"))
                                "Toggle navigation")
                          (span (@ (class "icon-bar")) " ")
                          (span (@ (class "icon-bar")) " ")
                          (span (@ (class "icon-bar")) " "))
                  (a (@ (class "navbar-brand")
                        (href  ,(wrap st8 "/"))) ,%title%))
             (div (@ (class "collapse navbar-collapse"))
                  (ul (@ (class "nav navbar-nav navbar-right"))
                      (,%navigation%
                       ,(if st8
                            `((li (a (@ (href  ,(wrap st8 "/session"))
                                        (title "Continue playing"))
                                     "Play"))
                              (li (a (@ (href  ,(wrap st8 "/account"))
                                        (title "Manage your account"))
                                     "My Account"))
                              (li (a (@ (href "/")
                                        (title "Log out"))
                                     "Sign out")))
                            `((li (a (@ (href  "/login")
                                        (title "Sign in to your account"))
                                     "Sign in"))
                              (li (a (@ (href  "/register")
                                        (title "Create a new account"))
                                     "Register"))))
                       (li (a (@ (href "#")
                                 (title "Hide Help"))
                              "Help  ^"))))))))
(define* (footer-region)
  `(div (@ (id "footer")) ,%footer%))
