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

(add-to-load-path (dirname (current-filename))) ; add guilecraft libs
(add-to-load-path (string-append (dirname (current-filename))
                                 "/artanis")) ; add artanis

((@ (guilecraft boot) boot) (program-arguments))

