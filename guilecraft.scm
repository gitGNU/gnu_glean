#! /usr/bin/guile -s
coding:utf-8
!#

;;; Launcher sets the loadpath to include guilecraft's 
;;; directory.
;;; For testing:
;;; Execute script, launch emacs, connect-to-guile, 
;;; load-file, eval-buffer.

(add-to-load-path "/home/alex/Projects/guilecraft/") ;location of guilecraft libs

((@ (guilecraft boot) boot) (program-arguments))


