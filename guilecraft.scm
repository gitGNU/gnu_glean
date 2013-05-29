#!/usr/bin/env guile
-s
coding:utf-8
!#

;;; Commentary:
;;
;; Launcher sets the loadpath to include guilecraft's
;; directory.
;; For testing:
;; Execute script, launch emacs, connect-to-guile,
;; load-file, eval-buffer.
;;
;;; Code:

(define %guilecraft-dir% "/home/trisquel/projects/guilecraft")

(add-to-load-path %guilecraft-dir%) ;location of guilecraft libs

((@ (guilecraft boot) boot) (program-arguments))
