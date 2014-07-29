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

((@ (glean boot) boot) (program-arguments))
