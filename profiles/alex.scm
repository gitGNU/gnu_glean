;;; guilecraft --- user profile data-types.         -*- coding: utf-8 -*-

(define-module (profiles alex)
  #:use-module (guilecraft gprofiles)
  #:use-module (guilecraft scorecards)
  #:use-module (modules git)
  #:use-module (modules obnam)
  #:export (alex-profile))

(define alex-profile
  (gprof_make-profile "130215alex"
		      "alex"
		      `(,git-gmodule ,obnam-gmodule)
		      `(,(scc_make-scorecard-datum git-gmodule
						   'git-branch
						   0)
			,(scc_make-scorecard-datum git-gmodule
						   'git-status
						   2)
			,(scc_make-scorecard-datum git-gmodule
						   'git-add
						   5))))
