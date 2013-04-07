;;; guilecraft --- user profile data-types.         -*- coding: utf-8 -*-

(define-module (profiles alex)
  #:use-module (guilecraft gprofiles)
  #:use-module (guilecraft scorecards)
  #:export (alex-profile))

(define alex-profile
  (gprof_make-profile "130215alex"
		      "alex"
		      '(git)
		      `(,(scc_make-scorecard-datum 'git
						   'git-branch
						   0)
			,(scc_make-scorecard-datum 'git
						   'git-status
						   2)
			,(scc_make-scorecard-datum 'git
						   'git-add
						   5))))
