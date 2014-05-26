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
  (get "/css/bootstrap-theme.css"
       (lambda (rc)
         (emit-response-with-file
          (string-append %guilecraft-dir%
                         "/www/css/bootstrap-theme.min.css"))))
  (get "/css/bootstrap.css"
       (lambda (rc)
         (emit-response-with-file
          (string-append %guilecraft-dir%
                         "/www/css/bootstrap.min.css"))))
  (get "/css/sticky-footer-navbar.css"
       (lambda (rc)
         (emit-response-with-file
          (string-append %guilecraft-dir%
                         "/www/css/sticky-footer-navbar.css"))))
  (get "/css/guilecraft.css"
       (lambda (rc)
         (emit-response-with-file
          (string-append %guilecraft-dir%
                         "/www/css/guilecraft.css"))))
  (get "/js/jquery.min.js"
       (lambda (rc)
         (emit-response-with-file
          (string-append %guilecraft-dir%
                         "/www/js/jquery.min.js"))))
  (get "/js/bootstrap.min.js"
       (lambda (rc)
         (emit-response-with-file
          (string-append %guilecraft-dir%
                         "/www/js/bootstrap.min.js"))))
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
    (get "/detail"
       (lambda (rc)
         (response-emit (tpl->html (detail rc)))))
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
              #:title "Guilecraft — My Account"
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
                  #:title "Guilecraft — Fallback Account Management"
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
        `(p "You have no active modules yet.")
        (render-modules active-modules "Active Modules" st8)))
  (form  "/mod-action"
         `((h2 "Activity")
           (p ,(sa "Select the modules in which you are"
                   " interested by checking their checkboxes."))
           (p "You can then enable them by clicking ‘Activate’.")
           ,(list-active)
           ,(list-available "Enable" st8)
           ,(state->form-fields st8)
           (input (@ (type  "hidden")
                     (name  "operation")
                     (value "activate-modules")))
           (button (@ (class "btn btn-primary")
                      (type  "submit"))
                   "Activate"))))

(define (detail rc)
  (define (render-set detail st8)
    (define* (render-if empty-pred obj title #:optional (renderer #f))
      (if (empty-pred obj)
          "" `(dt ,title (dd ,(if renderer (renderer obj) obj)))))
    (match detail
      ((hash id name version keywords synopsis description creator
             attribution resources module contents)
       (panel name
              `(dl
                (dt "Name"
                    (dd ,(if (string-null? name)
                             (symbol->string id)
                             (string-append name " (" version ")"))))
                ,(render-if null? keywords "Keywords"
                            (lambda (k) (string-join k ", ")))
                ,(render-if string-null? synopsis "Synopsis")
                ,(render-if string-null? description "Description")
                ,(render-if string-null? creator "Author(s)")
                ;; Tricky format? No plain string?
                ;; (dt "Attribution"
                ;;     (dd attribution))
                ;; (dt "Further Resources"
                ;;     (dd resources))
                ,(render-if (const #t) module "Selectable Set?"
                            (lambda (k) (if k "Yes" "No")))
                ,(render-if null? contents "Contents"
                            (lambda (k) (render-modules k
                                                        "Contents"
                                                        st8))))))))
  (let* ((st8 (query-string->state rc))
         (rsp (view-set (string->symbol (params rc "hash")) st8))
         (content (if (stateful? rsp)
                      (render-set (result rsp) (state rsp))
                      (nothing-handler rsp))))
    (frame
     #:state (if (stateful? rsp) (state rsp) #f)
     #:rc    rc
     #:title "Guilecraft — Set Details"
     #:page  `(,content))))

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
             ,(form "/eval-action"
                    `((input (@ (type        "text")
                                (placeholder "Enter your solution here…")
                                (name        "solution")
                                (class       "form-control")
                                (autofocus   "autofocus")))
                      ,(state->form-fields st8)
                      (button (@ (class "btn btn-primary")
                                 (type  "submit"))
                              "Solve"))))))
  (let* ((st8 (query-string->state rc))
         (rsp (next-challenge st8)))
    (frame
     #:state (if (stateful? rsp) (state rsp) #f)
     #:rc    rc
     #:title "Guilecraft — Next Challenge…"
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

(define* (render-modules modules title st8
                         #:optional (active #f) (format 'table))
  ;; format is ignored for now: defaults to table.
  (define (render-module module)
    (match module
      ((hash id name version keywords synopsis)
       `(tr
         ,(if active
              `((td (input (@ (type "checkbox")
                              (name ,hash)) " ")))
              " ")
         (td (p (a (@ (href ,(wrap st8 "/detail"
                                   (string-append
                                    "hash="
                                    (if (symbol? hash)
                                        (symbol->string hash)
                                        hash))))
                      (title "Click to view details about this set"))
                   ,(if (string-null? name)
                        (symbol->string id)
                        (string-append name " (" version ")")))))
         (td ,(if (null? keywords)
                  ""
                  `(p ,(string-append "Keywords: "
                                      (string-join keywords ", "))))
             (p ,synopsis))))))
  (panel title
         (if (nothing? modules)
             (nothing-handler modules)
             `(div (@ (class "table-responsive"))
                   (table
                    (@ (class "table table-stripped"))
                    (thead
                     (tr ,(if active `(th ,active) "")
                         (th "name (version)")
                         (th "synopsis")))
                    (tbody
                     ,(map render-module modules)))))))

(define* (list-available #:optional (active #f)
                         (st8 (mk-state 37146
                                        %lounge-port%
                                        %library-port%)))
  (let ((rsp (known-modules st8)))
    (cond ((stateful? rsp)
           (render-modules (result rsp) "Available Modules"
                           st8 active))
          (else
           (render-modules rsp "Available Modules"
                           st8 active)))))

(define* (frame #:key (state #f) (page `(p "This is a test page"))
                (title "Guilecraft — Fast Learning Tool")
                (rc #f))
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
                ((string=? msg "aut-success")
                 (alert "You are now logged in." 'success))
                ((string=? msg "reg-success")
                 (alert "Your account has been registered." 'success))
                (else
                 (alert "Success! But we don't know with what…"
                        'success)))
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
                   (type "text/css")
                   (href "/css/bootstrap.css")))
          (link (@ (rel  "stylesheet")
                   (type "text/css")
                   (href "/css/sticky-footer-navbar.css")))
          ;; Optional Theme
          ;; (link (@ (href "starter-template.css")
          ;;          (rel  "stylesheet")))>
          (link (@ (rel  "stylesheet")
                   (type "text/css")
                   (href "/css/bootstrap-theme.css")))
          (link (@ (rel  "stylesheet")
                   (type "text/css")
                   (href "/css/guilecraft.css")))
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
               (script (@ (src "/js/jquery.min.js"))
                       (type "text/javascript")
                       "test")
               ;; Include all compiled plugins (below), or include individual
               ;; files as needed

               ;; Latest compiled and minified JavaScript
               (script (@ (src "/js/bootstrap.min.js"))
                       (type "text/javascript")
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
                          (span (@ (class "icon-bar")) " ")
                          (span (@ (class "icon-bar")) " ")
                          (span (@ (class "icon-bar")) " "))
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

(define (nothing-handler nothing)
  (match (list (nothing-id nothing)
               (nothing-context nothing))
    ((noth-id noth-con)
     (cond ((eqv? 'servers-down noth-id)
            (alert "The lounge and library seem to be down." 'danger))
           ((eqv? 'lounge-down noth-id)
            (alert "The lounge at ~a is currently down." 'danger))
           ((eqv? 'library-down noth-id)
            (alert "The library at ~a is currently down." 'danger))
           (else
            (let ((neg-ori (neg-orig noth-con))
                  (neg-msg (neg-msg  noth-con)))
              (cond ((eqv? 'username-taken neg-msg)
                     (alert
                      "Your chosen username is no longer available."
                      'info))
                    ((eqv? 'invalid-username neg-msg)
                     (alert
                      "Your chosen username is not a valid user name."
                      'warning))
                    ((eqv? 'unknown-user neg-msg)
                     (alert
                      "The username you entered is unknown here."
                      'warning))
                    ((eqv? 'incorrect-password neg-msg)
                     (alert
                      "The password you supplied is not your currently
registered password."
                      'warning))
                    ((eqv? 'invalid-token neg-msg)
                     (alert "Your session is no longer valid." 'info))
                    ((eqv? 'no-active-modules neg-msg)
                     (alert "You have not yet enabled any modules. \
Please visit your account page where you will be able to enable some."
                            'info))
                    ((eqv? 'exchange-error noth-id)
                     (alert (string-append "We got a negative
response. Message: " (symbol->string neg-msg)) 'danger))
                    (else
                     (alert "Unknown error." 'danger)))))))))

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
  (let ((result (sa (if target (sa target "?") "")
                    "token=" (number->string (state-tk state))
                    "&lounge=" (state-lng state)
                    "&library=" (state-lib state)
                    (if extra (sa "&" extra) ""))))
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

(define* (form action contents #:optional (method "post"))
  "Return an sxml representation of a bootstrap form. ACTION should a
string URI. CONTENTS should be an SXML list."
  `(form (@ (action ,action)
            (method ,method)
            (role   "form"))
         ,contents))

(define* (alert message #:optional (type 'default))
  "Return an sxml representation of a bootstrap alert containing the
string MESSAGE. If TYPE is given it should be a symbol ('default,
'primary, 'success, 'info, 'warning or 'danger)."
  `(div (@ (class ,(string-append "alert alert-"
                                  (symbol->string type))))
        (p ,message)))
