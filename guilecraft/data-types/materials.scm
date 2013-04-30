;;; guilecraft ---          -*- coding: utf-8 -*-

(define-module (guilecraft data-types materials)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-materials))

;; Background Information: materials for self-directed
;; study or reference to origin of module.
(define-record-type <materials>
  (mat_make-materials identity reading video audio venue)
  materials?
  ;; Organization, group or person associated with resource
  (identity get-identity)
  ;; Reading: websites, books, pamphlet, etc.
  (reading get-reading)
  (video get-video)
  (audio get-audio)
  ;; Regular haunts: meetups, #channels, etc.
  (venue get-venue))

