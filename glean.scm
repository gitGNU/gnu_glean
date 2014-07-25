#!/usr/bin/env guile
-s
coding:utf-8
!#

;;; Commentary:
;;
;; Launcher sets the loadpath to include glean's
;; directory.
;; For testing:
;; Execute script, launch emacs, connect-to-guile,
;; load-file, eval-buffer.
;;
;;; Code:

;(add-to-load-path (dirname (current-filename))) ; add glean libs
;(add-to-load-path (string-append (dirname (current-filename))
;                                 "/artanis")) ; add artanis

((@ (glean boot) boot) (program-arguments))

