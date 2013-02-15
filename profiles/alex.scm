;;; guilecraft --- user profile data-types.         -*- coding: utf-8 -*-

(define-module (profiles alex)
  #:use-module (guilecraft gprofiles)
  #:export (alex-profile))

(define alex-profile
  (make-gprofile "130215alex"
		 "alex"
		 '(git-gmodule)
		 '(git-branch . 0)))
