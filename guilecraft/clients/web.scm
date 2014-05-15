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
;;; Provide a web user interface. It can be published to other network
;;; participants through a reverse proxy setup with other web servers
;;; (e.g. Apache, Nginx).
;;;
;;;; Code:

(define-module (guilecraft clients web)
  #:use-module (artanis artanis)
  #:use-module (guilecraft clients monadic-min)
  #:use-module (guilecraft config)
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft monads)
  #:use-module (guilecraft utils)
  #:use-module (ice-9 match)
  #:use-module (sxml simple)
  #:use-module (web uri)
  #:export (web-client))

;; For inclusion in Artanis
(define (params-raw rc)
  (unless (rc-qt rc) ((@@ (artanis artanis) init-query!) rc))
  (rc-qt rc))
(define (params-map proc rc)
  (map proc (params-raw rc)))
(define (params-filter pred rc)
  (filter pred (params-raw rc)))

;; Make text more easily fit the 72columns
(define (sa . args)
  (apply string-append args))

(define (web-client)

  (init-server)

  (get "/$"
       (lambda (rc)
         (response-emit (tpl->html (index rc)))))
  (get "/login"
       (lambda (rc)
         (let ((st8 (query-string->state rc)))
           (if st8
               (redirect-to rc (wrap st8 "/account"))
               (response-emit (tpl->html (login rc)))))))
  (get "/register"
       (lambda (rc)
         (let ((st8 (query-string->state rc)))
           (if st8
               (redirect-to rc (wrap st8 "/account"))
               (response-emit (tpl->html (register rc)))))))
  (get "/account"
       (lambda (rc)
         (let ((st8 (query-string->state rc)))
           (if st8
               (response-emit (tpl->html (account rc)))
               (redirect-to rc (wrap st8 "/login"))))))
  (get "/session"
       (lambda (rc)
         (response-emit (tpl->html (session rc)))))
  (get "/hello"
       (lambda (rc)
         (response-emit
          (tpl->html
           `((p "OK — Not yet implemented.")
             ;; (p "Query Table contents:")
             ;; (dl
             ;;  ,(params-map (lambda (kv-pair)
             ;;                 `((dt ,(car kv-pair))
             ;;                   (dd ,(cadr kv-pair))))
             ;;               rc)))))))
             )))))

  (post "/auth-action"
        (lambda (rc)
          (auth-action rc)))
  (post "/reg-action"
        (lambda (rc)
          (reg-action rc)))
  (post "/mod-action"
        (lambda (rc)
          (mod-action rc)))
  (post "/del-action"
        (lambda (rc)
          (del-action rc)))
  (post "/eval-action"
        (lambda (rc)
          (eval-action rc)))
  (run))

(define (index rc)
  (frame #:state (query-string->state rc)
         #:rc    rc
         #:page `((div (@ (class "row"))
                       (div (@ (class "col-md-8"))
                            (div (@ (class "jumbotron"))
                                 (h1 "Guilecraft")
                                 (p  "Test intro.")))
                       (div (@ (class "col-md-4"))
                            ,(login-block)))
                  (div (@ (class "row"))
                       (div (@ (class "col-md-8"))
                            ,(list-available))))))
(define (login rc)
  (frame #:state (query-string->state rc)
         #:title ("Guilecraft — Sign In")
         #:page  `(div (@ (class "starter-template"))
                       (h1 "Sign In")
                       (p "Sign in to your account.")
                       ,(login-block))))
(define (register rc)
  (frame #:state (query-string->state rc)
         #:title "Guilecraft — Register"
         #:page  `((h1 "Account Registration")
                   (p "Sign up to Guilecraft by completing this form.")
                   (form (@ (action "/reg-action")
                            (method "post")
                            (role   "form"))
                         (div (@ (class "form-group"))
                              (label (@ (for "username")) "Username:")
                              (input (@ (name        "username")
                                        (type        "text")
                                        (placeholder "Username")
                                        (class       "form-control"))))
                         (div (@ (class "form-group"))
                              (label (@ (for "password")) "Password (N/A):")
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
                                 "Sign Up")))))
(define (account rc)
  (let ((st8 (query-string->state rc)))
    (frame
     #:state st8
     #:rc    rc
     #:title "Guilecraft — My Account"
     #:page  `((h1 "Check Your Account Details")
               (h2 "Activity")
               (p ,(sa "Select the modules in which you are"
                       " interested by checking their checkboxes."))
               (p "You can then enable them by clicking ‘Activate’.")
               (form (@ (action "/mod-action")
                        (method "post")
                        (role   "form"))
                     ,(list-available 'active st8)
                     ,(state->form-fields st8)
                     (input (@ (type  "hidden")
                               (name  "operation")
                               (value "activate-modules")))
                     (button (@ (class "btn btn-primary")
                                (type  "submit"))
                             "Activate")
                     (button (@ (class "btn btn-primary")
                                (type  "reset"))
                             "Reset"))
               (h2 "Identity")
               (p "Username")
               (p "Password")
               (button (@ (class "btn btn-primary")
                          (type  "submit"))
                       "Update Identity")
               (h2 "Servers")
               (p "Lounge")
               (p "Library")
               (button (@ (class "btn btn-primary")
                          (type  "submit"))
                       "Update Servers")
               (h2 "Expiration")
               (p ,(sa "You can delete your account by clicking on"
                       " the button below. This action is immediate"
                       " and cannot be undone."))
               (form (@ (action "/del-action")
                        (method "post")
                        (role   "form"))
                     ,(state->form-fields st8)
                     (button (@ (class "btn btn-danger")
                                (type  "submit"))
                             "Delete Account"))))))
(define (session rc)
  (define (render-challenge question st8)
    (define (parse-q)
      (let ((media (q-media question)))
        `((p ,(sa "Question: " (q-text question)))
          ,(map (lambda (image)
                  `(img (@ (src ,image)
                           (width "300"))))
                (media-images media))
          ,(map (lambda (vid)
                  `(video (@ (src ,vid))))
                (media-videos media))
          ,(map (lambda (aud)
                  `(audio (@ (src ,aud))))
                (media-audio media))
          ,(map (lambda (url)
                  `(a (@ (href ,url))
                      ,url))
                (media-urls media))
          ,(map (lambda (book)
                  `(p ,book))
                (media-books media)))))
    (panel "Challenge"
           `(,(parse-q)
             (form (@ (action "/eval-action")
                      (method "post")
                      (role   "form"))
                   (input (@ (type        "text")
                             (placeholder "Enter your solution here…")
                             (name        "solution")
                             (class       "form-control")
                             (autofocus   "autofocus")))
                   ,(state->form-fields st8)
                   (button (@ (class "btn btn-primary")
                              (type  "submit"))
                           "Solve")))))
  (let* ((st8 (query-string->state rc))
         (rsp (next-challenge st8))
         (content (if (stateful? rsp)
                      (render-challenge (car (result rsp))
                                        (state rsp))
                      (nothing-handler rsp))))
    (frame
     #:state (if (stateful? rsp) (state rsp) #f)
     #:rc    rc
     #:title "Guilecraft — Next Challenge…"
     #:page  `(,content))))
(define (auth-action rc)
  (let* ((name     (params rc "username"))
         (password (params rc "password"))
         ;;(params rc "lounge") (params rc "library")
         (st8      (authenticate-player name %lounge-port%))) ; passwd
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
           (st8   (register-player name lng lib)))
      (post-auth st8 rc "reg-success"))))
(define (post-auth st8 rc msg)
  (if (state? st8)
      (redirect-to rc (wrap st8 "/account" (sa "result=" msg)))
      (response-emit
       (tpl->html
        ;;('reg-failed (redirect-to rc "/login?login_failed=true"))
        ;;('invalid-rq (redirect-to rc "/login"))
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
                   (frame #:page (nothing-handler rsp))))))))))
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
         (answer (uri-decode (params rc "solution")))
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
              #:title "Guilecraft — Incorrect Answer"
              #:page  (panel "Solution"
                             `((p ,(sa "Incorrect. You answered: " answer))
                               (p ,(sa "The solution is actually: "
                                       (s-text (cadr (result rsp)))))
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
  `(form (@ (action "/auth-action")
            (method "post")
            (role "form"))
         (div (@ (class "form-group"))
              (label (@ (for "username"))
                     "Username:")
              (input (@ (type "text")
                        (placeholder "Username")
                        (class "form-control")
                        (name "username"))))
         (div (@ (class "form-group"))
              (label (@ (for "password"))
                     "Password (N/A):")
              (input (@ (type "password")
                        (placeholder "Password")
                        (class "form-control")
                        (name "password"))))
         (button (@ (type "submit")
                    (class "btn btn-success"))
                 "Play")))

(define* (list-available #:optional (active #f)
                         (state (mk-state 'unimportant
                                          %lounge-port%
                                          %library-port%)))
  (define (gen-table modules)
    `(div (@ (class "table-responsive"))
          (table
           (@ (class "table table-stripped"))
           (thead
            (tr ,(if active `(th "enable") "")
                (th "name (version)")
                (th "synopsis")))
           (tbody
            ,(map (lambda (module)
                    `(tr
                      ;; hash
                      ,(if active
                           `(td (input (@ (type "checkbox")
                                          (name ,(car module)))
                                       " "))
                           "")
                      ;; name (version)
                      (td (p ,(sa (caddr module) " ("
                                  (cadddr module) ")")))
                      ;; synopsis
                      (td (p ,(cadddr (cdr module))))))
                  modules)))))

  (let ((rsp (known-modules state)))
    (panel "Available Modules"
           (cond ((stateful? rsp)
                  (gen-table (result rsp)))
                 (else
                  (nothing-handler rsp))))))

(define* (frame #:key (state #f) (page `(p "This is a test page"))
                (title "Guilecraft — Fast Learning Tool")
                (rc #f))
  (define (parse-msg)
    (let ((msg (if rc (params rc "result") #f)))
      (if msg
          (cond ((string=? msg "del-success")
                 `(div (@ (class "alert alert-success"))
                       "Your account has been deleted."))
                ((string=? msg "add-success")
                 `(div (@ (class "alert alert-success"))
                       "Your selected modules have been activated."))
                ((string=? msg "aut-success")
                 `(div (@ (class "alert alert-success"))
                       "You are now logged in."))
                ((string=? msg "reg-success")
                 `(div (@ (class "alert alert-success"))
                       "Your account has been registered."))
                (else
                 `(div (@ (class "alert alert-success"))
                       "Success! But we don't know with what…")))
          "")))

  `(html (@ (lang "en"))
         (head
          (meta (@ (charset    "utf-8")))
          (meta (@ (http-equiv "X-UA-Compatible")
                   (content    "IE=edge")))
          (meta (@ (name    "viewport")
                   (content "width=device-width, initial-scale=1")))
          (title ,title)
          ;; Minified bootstrap css
          (link (@ (rel  "stylesheet")
                   (href
                    "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css")))
          (link (@ (rel  "stylesheet")
                   (href
                    "http://getbootstrap.com/examples/sticky-footer-navbar/sticky-footer-navbar.css")))
          ;; Optional Theme
          ;; (link (@ (href "starter-template.css")
          ;;          (rel  "stylesheet")))>
          (link (@ (rel  "stylesheet")
                   (href
                    "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css")))
          ;; <!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->
          ;; <!--[if lt IE 9]>
          ;;   <script src="https://oss.maxcdn.com/libs/html5shiv/3.7.0/html5shiv.js"></script>
          ;;   <script src="https://oss.maxcdn.com/libs/respond.js/1.4.2/respond.min.js"></script>
          ;; <![endif]-->
          )
         (body (@ (role "document"))
               ,(header-region state)
               (div (@ (class "container")
                       (role  "main"))
                    ,(parse-msg)
                    ,page)
               ,(footer-region)
               ;; jQuery (necessary for Bootstrap's JavaScript plugins)
               (script (@ (src
                           "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"))
                       "test")
               ;; Include all compiled plugins (below), or include individual
               ;; files as needed

               ;; Latest compiled and minified JavaScript
               (script (@ (src
                           "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"))
                       "test"))))

(define* (header-region #:optional st8)
  `(div (@ (class "navbar navbar-inverse navbar-fixed-top")
           (role  "navigation"))
        (div (@ (class "container"))
             (div (@ (class "navbar-header"))
                  (button (@ (type        "button")
                             (class       "navbar-toggle")
                             (data-toggle "collapse")
                             (data-target ".navbar-collapse"))
                          (span (@ (class "sr-only"))
                                "Toggle navigation")
                          (span (@ (class "icon-bar")) "a")
                          (span (@ (class "icon-bar")) "a")
                          (span (@ (class "icon-bar")) "a"))
                  (a (@ (class "navbar-brand")
                        (href  ,(wrap st8 "/")))
                     "Guilecraft"))
             (div (@ (class "collapse navbar-collapse"))
                  (ul (@ (class "nav navbar-nav navbar-right"))
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
                           `(li (a (@ (href  "/register")
                                      (title "Create a new account"))
                                   "Register"))))))))
(define* (footer-region)
  `(div (@ (id "footer"))
        (div (@ (class "container"))
             (p (@ (style "text-align:center;padding-top:1em"))
                "Copyright © 2014 Alex Sassmannshausen"))))

(define (nothing-handler rsp)
  (let ((noth-id   (nothing-id rsp))
        (noth-con  (nothing-context rsp)))
    (cond ((eqv? 'servers-down noth-id)
           '(div (@ (class "alert alert-danger"))
                 (p "The lounge and library seem to be down: ~a.")))
          ((eqv? 'lounge-down noth-id)
           `(div (@ (class "alert alert-danger"))
                 (p "The lounge at ~a is currently down.")))
          ((eqv? 'library-down noth-id)
           `(div (@ (class "alert alert-danger"))
                 (p "The library at ~a is currently down.")))
          (else
           (let ((neg-ori (neg-orig noth-con))
                 (neg-msg (neg-msg  noth-con)))
             (cond ((eqv? 'name-taken neg-msg)
                    `(div (@ (class "alert alert-info"))
                          (p "'~a' is no longer available. ")))
                   ((eqv? 'invalid-username neg-msg)
                    `(div (@ (class "alert alert-warning"))
                          (p "'~a' is not a valid user name. ")))
                   ((eqv? 'unknown-name neg-msg)
                    `(div (@ (class "alert alert-warning"))
                          (p "'~a' is unknown at this lounge. ")))
                   ((eqv? 'invalid-token neg-msg)
                    `(div (@ (class "alert alert-info"))
                          (p "Your session is no longer valid. ")))
                   ((eqv? 'no-active-modules neg-msg)
                    `(div (@ (class "alert alert-info"))
                          (p "You have not yet enabled any modules.")
                          (p (sap "Please visit your account page"
                                  " where you will be able to enable"
                                  " modules."))))
                   ((eqv? 'exchange-error noth-id)
                    `(div (@ (class "alert alert-danger"))
                          (p "We got a negative response.")))
                   (else
                    `(div (@ (class "alert alert-danger"))
                          (p "Unknown error.")))))))))

(define (state->form-fields state)
  `(fieldset
    (input (@ (type "hidden")
              (name "token")
              (value ,(number->string (state-tk state)))))
    (input (@ (type "hidden")
              (name "lounge")
              (value ,(state-lng state))))
    (input (@ (type "hidden")
              (name "library")
              (value ,(state-lib state))))))

(define* (wrap state target #:optional (extra #f))
  (if state
      (state->query-string state target extra)
      target))
(define* (state->query-string state #:optional (target #f) (extra #f))
  (format #t "state: ~a\ntarget: ~a\n" (object->string state) target)
  (format #t "extra: ~a\n" (object->string extra))
  (let ((result (sa (if target (sa target "?") "")
                    "token=" (number->string (state-tk state))
                    "&lounge=" (state-lng state)
                    "&library=" (state-lib state)
                    (if extra (sa "&" extra) ""))))
    (format #t "~a\n" result)
    result))

(define (query-string->state rc)
  (let ((tk  (params rc "token"))
        (lng (params rc "lounge"))
        (lib (params rc "library")))
    (if (and tk lng lib)
        (mk-state (string->number tk)
                  (uri-decode lng)
                  (uri-decode lib))
        #f)))

;;;;; Bootstrap Helpers
(define* (panel heading contents #:optional (type 'default))
  "Return an sxml representation of a bootstrap panel containing the
string HEADING and the sxml value CONTENTS. If TYPE is given it should
be a symbol ('default, 'primary, 'success, 'info, 'warning or
'danger)."
  `(div (@ (class ,(sa "panel panel-" (symbol->string type))))
          (div (@ (class "panel-heading"))
               (h3 (@ (class "panel-title")) ,heading))
          (div (@ (class "panel-body"))
               ,contents)))
