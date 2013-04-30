;;; guilecraft --- Nix package management from Guile.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types credits)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (cred_make-credit))

;; Accreditation: Give creds where they are due
(define-record-type <credit>
  (cred_make-credit current-creator past-creator contributor)
  credit?
  ;; The current maintainer of the module
  (current-creator get-current-creator)           ; string
  ;; Honor past work
  (past-creator get-past-creator)              ; string
  ;; regular contributor
  (contributor get-contributor))              ; string

