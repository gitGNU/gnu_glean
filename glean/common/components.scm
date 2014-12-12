;; components.scm --- handle extensions  -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen  <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
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
;; A module providing component configuration and extensibility features.
;;
;; Primary uses are:
;; - when writing components: COMPONENT, CONFIG, SETTING, PRIMARY-CONFIG.
;; - when embedding components in a glean server: COMPONENT-NODE.
;;
;;; Code:

(define-module (glean common components)
  #:use-module (glean common config-utils)
  #:use-module (glean common utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-9)
  #:export (
            ;; Component Creation
            define-component
            config
            setting
            primary-config
            ;; Component Use
            component-node
            ))


;;;; Component Creation

;;;;; <component>
;;; A Component Record specifies a pluggable feature for glean. It's context
;;; is the product of its definition and the 'component-node' (see below)
;;; which eventually loads it, as its paths are interpreted relatively to the
;;; extensions directory of 'component-node'.
;;; - NAME: the Scheme name of the component as string (e.g. "local-client").
;;; - PROCEDURE: the workhorse, the single point of entry for the component,
;;;              returned by 'component-node', when the component is loaded.
;;; - DIRS: extra directories to be created below the extension directory.
;;; - FILES: configuration or empty files to be created (but not necessarily
;;;          loaded).
;;; - CONFIG: configuration files to be loaded before PROCEDURE is returned.
;;;

;;;;; Example:
;;; (define test-client
;;;   (make-component "test-client")
;;;                   test-procedure
;;;                   (list "files" "css" "js")
;;;                   (list css-config js-config main-config)
;;;                   (list main-config)))
;;; Note that these should not be used by the user directly. Instead, use the
;;; 'porcelain procedures' below.

(define-record-type <component>
  (make-component name procedure dirs files config)
  component?
  (name        component-name)
  (procedure   component-procedure)
  (dirs        component-dirs)
  (files       component-files)
  (config      component-config))

(define-record-type <component-config>
  (make-comp-config name lang settings path load)
  component-config?
  (name      comp-config-name)
  (lang      comp-config-lang)
  (settings  comp-config-settings)
  (path      comp-config-path)
  (load      comp-config-load))


;;;;; Porcelain: User Convenience
;;; The procedures provide user-friendly interfaces to creating
;;; components. For an example, see the 'define-component' docstring.

(define* (config #:key (name #f) (lang 'scheme) (settings '())
                 (path #f) (load #f))
  "Return an auxiliary config ready for inclusion in a component definition.

NAME is a mandatory string, which should include no whitespace and no
non-alphanumeric characters except for hyphens.

LANG is a symbol indicating the language of the configuration file. It
defaults to 'scheme.  For now only 'scheme is supported. Any other language
will force the file to start empty.

SETTINGS is a list of records, which are produced by using the 'make-setting'
function, defaulting to the empty list.

PATH is an optional string indicating a relative path from the component
data-directory (e.g. \"css/style.css\"). If it is ommitted it will default to
\"NAME.conf\".

If load is set to #t this config will be loaded before the associated
component's procedure is returned."
  (if (not name)
      (error "CONFIG -- Name is not specified.")
      (make-comp-config name lang settings path load)))

(define* (primary-config settings)
  "Return a config object ready for inclusion in a component definition, with
its config parameters set suitably for a component's primary configuration
file. See the documentation for 'config' for further details."
  (make-comp-config #f 'scheme settings #f #t))

(define* (setting #:key name (default 'glean-false) docstring)
  "Return a setting, suitable for inclusion in config records. NAME should be
a string representing a valid Guile variable name. DEFAULT is the dafault
value for the setting and DOCSTRING should be a string explaining what this
setting does."
  (if (and name (not (eqv? default 'glean-false)) docstring)
      (make-setting name default docstring)
      (error "SETTING -- NAME, DEFAULT or DOCSTRING missing.")))


(define* (define-component #:key name provides directories uses)
  "Return a fully-fledged component record built from NAME, PROVIDES,
DIRECTORIES, and USES.

Example:
 (component #:name        \"test-client\"
            #:provides    (lambda (component-args) 'do-stuff)
            #:directories (list \"files\" \"css\" \"js\")
            #:uses        (list
                           (config #:name     \"base.css\"
                                   #:lang     'css
                                   #:settings '()
                                   #:path     \"css/base.css\"
                                   #:load     #f)
                           (config #:name     \"base.js\"
                                   #:lang     'js
                                   #:settings '()
                                   #:path     \"js/base.js\"
                                   #:load     #f)
                           (primary-config
                            (list 
                              (setting #:name      \"default-title\"
                                       #:default   \"Glean Test Client\"
                                       #:docstring \"Title to show.\")))))
"

  (define (handle-with-languages comp-config)
    "Here we determine whether this config is a scheme file. If so we can do
some nice docstring etc printing. If not then we can't currently add any
content whatsoever. So we must force config-utils to create empty files out of
this config. The only way to do this is to go for the clumsy #f name, but
specify some form of path.

Normally languages should be handled in config-utils, using comment
characters. This will be done shortly."
    (if (eqv? comp-config-lang 'scheme)
        (component-aux-config (comp-config-name comp-config)
                              (comp-config-settings comp-config)
                              (comp-config-path comp-config))
        (component-aux-config #f
                              (comp-config-settings comp-config)
                              (if (comp-config-path comp-config)
                                  (comp-config-path comp-config)
                                  (comp-config-name comp-config)))))
  (define (uses->files comp-config)
    "Config-utils needs the contents of the files field to be its config
format. This turns primary-configs and other comp-configs into those."
    (cond ((primary-config? comp-config)
           (component-main-config name
                                  (comp-config-settings comp-config)))
          ((component-config? comp-config)
           (handle-with-languages comp-config))
          (else (error "COMPONENT -- uses contains invalid comp-config."))))
  (define (uses->config comp-config)
    "Config-utils expect the final field to be a list of paths to config files
to load. Extract the path from COMP-CONFIG, or build it from its name and
return it."
    (cond ((primary-config? comp-config)
           (string-append name ".conf"))
          ((component-config? comp-config)
           (if (string? (comp-config-path comp-config))
               (comp-config-path comp-config)
               (string-append (comp-config-name comp-config)
                              ".conf")))
          (else (error "COMPONENT -- uses contains invalid comp-config."))))
  (define (loader? obj) (and (component-config? obj) (comp-config-load obj)))
  (define (make)
    "Manufacture a fully-fledged component record out of our convenience layer."
    (let ((uses-list? (list? uses)))
      (make-component name provides directories
                      (if uses-list?
                          (map uses->files uses)
                          (uses->files uses))
                      (if uses-list?
                          (map uses->config
                               (filter loader? uses))
                          (uses->config uses)))))

  (if (and (string? name)
           (procedure? provides)
           (or (list? directories) (string? directories))
           (or (list? uses)
               (component-config? uses)))
      (make)
      (error "COMPONENT -- Not a valid component.")))


(define (primary-config? object)
  "Return #t if object is a config object as returned by 'primary-config'."
  (and (component-config? object)
       (and (not (comp-config-name object))
            (eqv? (comp-config-lang object) 'scheme)
            (not (comp-config-path object))
            (comp-config-load object))))

(define* (component-main-config component-name settings)
  "Return a config object as expected by config-utils, with COMPONENT-NAME
used as the base for its path."
  (make-config component-name (string-append component-name ".conf")
               settings))

(define* (component-aux-config aux-name settings #:optional aux-path)
  "Return a config object as expected by config-utils, with AUX-NAME providing
the foundation of the config's path if AUX-PATH is #f."
  (make-config aux-name (if aux-path
                            aux-path
                            (string-append aux-name ".conf"))
               settings))


;;;;; Component Loading

(define (component-node core-root core-dir extensions-root extensions-dir
                        default-component)
  "Return an interface with components loaded from the list COMPONENTS-DIR,
identifying different directories to scan for components, which when called
upon will run with DEFAULT-COMPONENT or a supplied component.

COMPONENTS-DIR should include the core directory, as well as any recognised
user component directories."
  (let ((components vlist-null))
    (define (reload)
      (set! components (load-components extensions-root extensions-dir
                                        "-ext.scm"
                                        (load-components core-root core-dir
                                                         "-core.scm"
                                                         vlist-null))))
    (define (get-raw component-name)
      (vhash-assoc component-name components))
    (define (get component-name)
      "Return the component identified by COMPONENT-NAME after preparing its
configs and files and loading the configs."
      (match (get-raw component-name)
        ((component-name . (mod-name . (? promise? component)))
         (match (force component)
           (($ <component> name procedure dirs files configs)
            (build/load-component extensions-dir name dirs files configs mod-name)
            procedure)
           (_ (error "COMPONENT NODE -- component is not a component"))))
        (_ 'error)))
    (define* (dispatch msg #:optional (component-name default-component))
      (cond ((eqv? msg 'get)     (get component-name))
            ((eqv? msg 'reload)  (reload))
            ((eqv? msg 'get-raw) (get-raw component-name))
            ((eqv? msg 'raw)     components)
            (else (error "COMPONENT NODE -- Unknown msg"))))

    (if (not (member extensions-root %load-path))
        (begin (mkdir-p extensions-root)
               (add-to-load-path extensions-root)))
    (reload)
    dispatch))


;;;;; Helpers

(define (build/load-component extensions-dir name dirs files configs mod-name)
  "Ensure the component identified by NAME and its config files and directories
named in DIRS and FILES exist, relative to EXTENSIONS-DIR, then load CONFIGS
relative to EXTENSIONS-DIR, in the (glean config) context."
  (let ((extension-root (string-append extensions-dir "/" name)))
    (define (wrap-then-act action)
      (lambda (path/config)
        (action
         (cond ((string? path/config)
                (string-append extension-root "/" path/config))
               ((configuration? path/config)
                (make-config (config-name path/config)
                             (string-append extension-root "/"
                                            (config-target path/config))
                             (config-contents path/config)))
               (else
                (error "BUILD/LOAD-COMPONENT -- unexpected path/config."))))))
    (define (do-list-or-file action object)
      (if (list? object)
          (for-each (wrap-then-act action) object)
          ((wrap-then-act action) object)))
    (mkdir-p extension-root)
    (do-list-or-file ensure-user-dirs dirs)
    (do-list-or-file ensure-config files)
    (do-list-or-file (lambda (config) (load-config config mod-name))
                     configs)))

(define (module-name file glean-dir)
  "Return the Guile module name for the file indicated by FILE, with
COMPONENTS-DIR as its base."
      (define not-slash (char-set-complement (char-set #\/)))
      (define prefix-len (string-length glean-dir))
      (define postfix-len (string-length ".scm"))
      (map string->symbol
           (string-tokenize (string-drop-right (string-drop file prefix-len)
                                               postfix-len)
                            not-slash)))


(define (load-components base-dir scan-dir pattern ext-so-far)
  "Return a new vhash based on COMPONENTS, consisting of all component
 records derived from component files in COMPONENTS-DIR."
  (define (leaf path stat result)
    "See if leaf is an component.  If so, add promise to return procedure
component from leaf, indexed by the component's name, to components.  If not,
simply return result."
    (let ((match (string-match pattern path)))
      (if (regexp-match? match)
          (let* ((ext-name (basename (match:prefix match)))
                 (mod-name (module-name path base-dir)))
            (insist (_ "Found component ~s: [add]~%") ext-name)
            (vhash-cons ext-name
                        (cons mod-name
                              (delay (module-ref (resolve-interface mod-name)
                                                 'component)))
                        result))
          (begin
            (insist (_ "Found non-component ~s: [skip]~%") (basename path))
            result))))

  (insist (_ "Scanning for ~s...~%") pattern)
  (component-walk scan-dir ext-so-far leaf))

(define (component-walk scan-dir ext-so-far leaf-proc)
  "Return EXT-SO-FAR, augmented by the result of applying LEAF-PROC as the
leaf procedure in a file-system-fold starting at SCAN-DIR."
  (file-system-fold (const #t)                         ; Enter?
                    leaf-proc                          ; Leaf
                    (lambda (path stat result) result) ; down
                    (lambda (path stat result) result) ; up
                    (const #f)                         ; skip
                    (lambda (path stat errno result)   ; error
                      (caution (_ "cannot access `~a': ~a.~%")
                               path (strerror errno))
                      result)
                    ext-so-far
                    scan-dir))

;;; component-utils.scm ends here
