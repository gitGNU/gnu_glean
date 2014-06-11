;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-
;;
;; Copyright © 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; This file is part of Guilecraft.
;;
;; Guilecraft is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3 of the License, or (at your
;; option) any later version.
;;
;; Guilecraft is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Guilecraft; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;; Commentary:
;;
;; A library providing a basic convenience layer between bootstrap and sxml. I
;; envisage procedures from this library to provide bootstrap building blocks,
;; which can be composed and combined to form individual pages of wildly
;; differing formats.
;;
;; Code:

(define-module (guilecraft clients web bootstrap)
  #:use-module (artanis artanis)
  #:use-module (guilecraft clients monadic-min)
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft monads)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:export (
            artanis-dispatch
            ;; Artanis
            params-raw
            params-map
            params-filter
            ;; Foundation
            state->form-fields
            wrap
            state->query-string
            query-string->state
            frame-maker
            nothing-handler
            ;; Views
            render-modules
            render-set
            ;; Bootstrap
            panel
            form
            alert
            video-source
            image-source
            website-source
            ))

;;;; Web Handler
(define* (artanis-dispatch resource-dir #:key index login register account
                           session detail auth-action reg-action mod-action
                           del-action eval-action)
  (define (source path)
    (lambda (rc)
      (emit-response-with-file (string-append resource-dir path))))
  (define (act proc)
    (if (procedure? proc)
        (lambda (rc) (proc rc))
        (present "Error: action not implemented.")))
  (define (present thing)
    (cond ((procedure? thing)
           (lambda (rc) (response-emit (tpl->html (thing rc)))))
          ((string? thing)
           (lambda (rc) (response-emit thing)))
          (else
           (lambda (rc) (response-emit "Error: not implemented.")))))
  (define (maybe-present path/proc-pair)
    (lambda (rc)
      (let ((st8 (query-string->state rc)))
        (match path/proc-pair
          (((? string? path) . (? procedure? proc))
           (if st8
               (redirect-to rc (wrap st8 path))
               (response-emit (tpl->html (proc rc)))))
          (((? procedure? proc) . (? string? path))
           (if st8
               (response-emit (tpl->html (proc rc)))
               (redirect-to rc (wrap st8 path))))
          (_ (present "Error: maybe-present not implemented."))))))

  (init-server)

  (get "/$" (present index))
  (get "/session" (present session))
  (get "/detail" (present detail))
  (get "/login" (maybe-present login))
  (get "/register" (maybe-present register))
  (get "/account" (maybe-present account))

  (post "/auth-action" (act auth-action))
  (post "/reg-action" (act reg-action))
  (post "/mod-action" (act mod-action))
  (post "/del-action" (act del-action))
  (post "/eval-action" (act eval-action))

  ;; FIXME: Resources do not yet include custom css and js.
  ;; Also, due to, I believe, bug in Artanis, they do not currently work at
  ;; all.
  ;; Use Apache rewrites instead.
  (get "/css/bootstrap-theme.css"
       (source "/www/css/bootstrap-theme.min.css"))
  (get "/css/sticky-footer-navbar.css"
       (source "/www/css/sticky-footer-navbar.css"))
  (get "/css/bootstrap.css" (source "/www/css/bootstrap.min.css"))
  (get "/css/guilecraft.css" (source "/www/css/guilecraft.css"))
  (get "/js/jquery.min.js" (source "/www/js/jquery.min.js"))
  (get "/js/bootstrap.min.js" (source "/www/js/bootstrap.min.js"))
  (get "/js/guilecraft.js" (source "/www/js/guilcraft.js"))

  (run))

;;;; For inclusion in Artanis
(define (params-raw rc)
  (unless (rc-qt rc) ((@@ (artanis artanis) init-query!) rc))
  (rc-qt rc))

(define (params-map proc rc)
  (map proc (params-raw rc)))

(define (params-filter pred rc)
  (filter pred (params-raw rc)))

;;;; Foundation
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
  (string-append (if target (string-append target "?") "")
                 "token=" (number->string (state-tk state))
                 "&lounge=" (state-lng state)
                 "&library=" (state-lib state)
                 (if extra (string-append "&" extra) "")))

(define (query-string->state rc)
  (let ((tk  (params rc "token"))
        (lng (params rc "lounge"))
        (lib (params rc "library")))
    (if (and tk lng lib)
        (mk-state (string->number tk)
                  (uri-decode lng)
                  (uri-decode lib))
        #f)))

(define (frame-maker base-url)
  "Return a procedure which takes state, page title and rc, and which in turn
returns an sxml representation of a bootstrap ready page.  BASE-URL should be
a string representing the base-url of the project. The CSS and JS will be
loaded relative to it."
  (define (resource path) (string-append base-url path))

  (lambda* (#:optional (page `(p "This is a test page"))
                       (title "Glean Knowledge Freely"))
    "Return an sxml representation of a bootstrap ready page, with the sxml
PAGE inserted right after the body element, and TITLE acting as the title of
the page."
    `(html (@ (lang "en"))
           (head
            (meta (@ (charset    "utf-8")))
            (meta (@ (http-equiv "X-UA-Compatible")
                     (content    "IE=edge")))
            (meta (@ (name    "viewport")
                     (content "width=device-width, initial-scale=1")))
            (title ,title)              ; insert TITLE here
            ;; CSS Includes.
            ;; Minified bootstrap css
            (link (@ (rel  "stylesheet")
                     (type "text/css")
                     (href ,(resource "/css/bootstrap.min.css"))))
            ;; Optional Theme
            (link (@ (rel  "stylesheet")
                     (type "text/css")
                     (href ,(resource "/css/bootstrap-theme.min.css"))))
            (link (@ (rel  "stylesheet")
                     (type "text/css")
                     (href ,(resource "/css/guilecraft.css"))))
            (link (@ (rel  "stylesheet")
                     (type "text/css")
                     (href ,(resource "/css/custom.css"))))
            ;; <!-- [if lt IE 9] >
            ;;    <script src="https://oss.maxcdn.com/libs/html5shiv/3.7.0/html5shiv.js"></script>
            ;;    <script src="https://oss.maxcdn.com/libs/respond.js/1.4.2/respond.min.js"></script>
            ;; <![endif]-->
            )
           ;; Body Proper.
           (body (@ (role "document"))
                 ,page                  ; insert PAGE here.
                 ;; JavaScript Includes.
                 ;; jQuery (necessary for Bootstrap's JavaScript plugins)
                 (script (@ (src ,(resource "/js/jquery.min.js"))
                            (type "text/javascript"))
                         " ")
                 ;; Include all compiled plugins (below), or include individual
                 ;; files as needed

                 ;; Latest compiled and minified JavaScript
                 (script (@ (src ,(resource "/js/bootstrap.min.js"))
                            (type "text/javascript"))
                         " ")
                 (script (@ (src ,(resource "/js/guilecraft.js"))
                            (type "text/javascript"))
                         " ")
                 (script (@ (src ,(resource "/js/custom.js"))
                            (type "text/javascript"))
                         " ")))))

(define (nothing-handler nothing)
  (match (list (nothing-id nothing)
               (nothing-context nothing))
    ((noth-id noth-con)
     (cond ((eqv? 'servers-down noth-id)
            (alert "The lounge and library which you make use of seem
to currently be down. Please try again later." 'warning))
           ((eqv? 'lounge-down noth-id)
            (alert "The lounge you are registered with seems to
currently be down. Please try again later." 'warning))
           ((eqv? 'library-down noth-id)
            (alert "The library which you make use of seems to
currently be down. Please try again later." 'warning))
           (else
            (let ((neg-ori (neg-orig noth-con))
                  (neg-msg (neg-msg  noth-con)))
              (cond ((eqv? 'username-taken neg-msg)
                     (alert
                      "Your chosen username is no longer available."
                      'warning))
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
response. Message: " (object->string neg-msg)) 'danger))
                    (else
                     (alert "Unknown error." 'danger)))))))))

;;;; Views Components
(define* (render-modules modules title st8
                         #:optional (active #f) (format 'table))
  ;; format is ignored for now: defaults to table.
  (define (render-module module)
    (match module
      ((hash id name version keywords synopsis logo)
       (cond
        ((eqv? format 'table)
         `(tr
           ,(if active
                `((td (input (@ (type "checkbox")
                                (name ,hash)) " ")))
                " ")
           (td ,(medium-logo logo name))
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
               (p ,synopsis))))
        (else
         `(li ,(medium-logo logo name)
              (h3 (a (@ (href ,(wrap st8 "/detail"
                                     (string-append
                                      "hash="
                                      (if (symbol? hash)
                                          (symbol->string hash)
                                          hash))))
                        (title "Click to view details about this set"))
                     ,(if (string-null? name)
                          (symbol->string id)
                          (string-append name " (" version ")"))))
              ,(if (null? keywords)
                   ""
                   `(p (@ (class "keywords"))
                       ,(string-append "Keywords: "
                                       (string-join keywords ", "))))
              (p (@ (class "synopsis")) ,synopsis)))))))
  (panel title
         (if (nothing? modules)
             (nothing-handler modules)
             (cond ((eqv? format 'table)
                    `(div (@ (class "table-responsive"))
                          (table
                           (@ (class "table table-stripped"))
                           (thead
                            (tr ,(if active `(th ,active) "")
                                (th "logo")
                                (th "name (version)")
                                (th "synopsis")))
                           (tbody
                            ,(map render-module modules)))))
                   (else `(div (@ (class "modules"))
                               (ul ,(map render-module modules))))))))

(define (render-set detail st8)
  (define* (render-if empty-pred obj title #:optional (renderer #f))
    (if (empty-pred obj)
        "" `(dt ,title (dd ,(if renderer (renderer obj) obj)))))
  (match detail
    ((hash id name version keywords synopsis description creator
           attribution resources properties contents logo)
     (panel name
            `(,(large-logo logo name)
              (dl
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
               ,(render-if (const #f) properties "Selectable Set?"
                           (lambda (k)
                             (if (assoc 'module k) "Yes" "No")))
               ,(render-if null? contents "Contents"
                           (lambda (k) (render-modules k
                                                       "Contents"
                                                       st8
                                                       #f 'list)))))))))

;;;; Bootstrap Components
(define* (panel heading contents #:optional (type 'default))
  "Return an sxml representation of a bootstrap panel containing the
string HEADING and the sxml value CONTENTS. If TYPE is given it should
be a symbol ('default, 'primary, 'success, 'info, 'warning or
'danger)."
  `(div (@ (class ,(string-append "panel panel-" (symbol->string type))))
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

(define (logo img name class width)
  (if (string-null? img)
      ""
      `(img (@ (src   ,img)
               (alt   ,(string-append "Logo of " name))
               (class ,class)
               (width ,width)))))

(define (medium-logo img name)
  (logo img name "set-logo-medium" "100"))

(define (large-logo img name)
  (logo img name "set-logo-large" "150"))

(define (video-source url)
  "Embed videos from a variety of sources on the basis of URL."
  (define (vimeo)
    `((iframe (@ (src ,url)
                 (class "video")
                 (width "500")
                 (height "300")
                 (frameborder "0")
                 (webkitallowfullscreen "1")
                 (mozallowfullscreen "1")
                 (allowfullscreen "1"))
              " ")))
  (vimeo))

(define (image-source url)
  "Embed images from a variety of sources on the basis of URL."
  `(img (@ (src ,url)
           (width "300")
           (class "image"))))

(define (website-source url)
  "Return an sxml representation of URL embedded in an iframe."
  `(iframe (@ (src ,url)
              (class "website"))
           " "))
