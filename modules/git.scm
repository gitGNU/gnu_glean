;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (modules git)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft problem-types open-problems)
  #:use-module (guilecraft problem-types multi-choice-problems)
  #:use-module (guilecraft gmodule-manager)
  #:export (git-gmodule))

(define git-init
  (gset_make-gset 'git-init
		  `(,(make-open-problem "What git command would you use to initiate a new project in the current directory?"
					   "git init")
		    ,(make-multi-choice-problem "In order to create a new git repository, would you:"
						    (cons "b" "type git init")
						    (cons "a" "type git test")
						    (cons "b" "type git init")
						    (cons "c" "type rm -r *")))))
(define git-status
  (gset_make-gset 'git-status
		  `(,(make-open-problem "How would you get a quick overview of the current status of the project in your current directory?"
					   "git status"))))
(define git-add
  (gset_make-gset 'git-add
		  `(,(make-open-problem "How would you add all newly created files & folders to the index of your project in the current directory?"
					   "git add .")
		    ,(make-open-problem "How would you add foo to the index of your project in the current directory?"
					   "git add foo")
		    ,(make-open-problem "How would you add foo/bar to index of your project in the current directory?"
					   "git add foo/bar"))))
;; Many revision control systems provide an add command that tells the system to start
;; tracking changes to a new file. Git’s add command does something simpler and more
;; powerful: git add is used both for new and newly modified files, and in both cases it
;; takes a snapshot of the given files and stages that content in the index, ready for
;; inclusion in the next commit.

(define git-commit
  (gset_make-gset 'git-commit
		  `(,(make-open-problem "How would you commit your changes to the git repository for your current project?"
					   "git commit")
		    ,(make-open-problem "What is a git shortcut for both adding your recent changes to the index and committing the index to the repository? (this will not add any newly createy files in the project structure to the repository!)"
					   "git commit -a"))))

;; A note on commit messages: Though not required, it's a good idea to
;; begin the commit message with a single short (less than 50 character)
;; line summarizing the change, followed by a blank line and then a more
;; thorough description. Tools that turn commits into email, for example,
;; use the first line on the Subject: lin and the rest of the commit in
;; the body.

(define git-diff
  (gset_make-gset 'git-diff
		  `(,(make-open-problem "How can you tell what you are ready to commit to your git repository in the current directory?"
					   "git diff --cached")
		    ,(make-open-problem "How can you tell what changes have been made since your last commit/add?"
					   "git diff"))))

(define git-log
  (gset_make-gset 'git-log
		  `(,(make-open-problem "How can you view the history of your changes?"
					   "git log")
		    ,(make-open-problem "How can you vie the complete diffs at each step of your history?"
					   "git log -p")
		    ,(make-open-problem "How can you get an overview of the changes of each step?"
					   "git log --stat --summary")
		    )))

(define git-branch
  (gset_make-gset 'git-branch
		  `(,(make-open-problem "How would you branch your current project to create a new branch named 'experimental'?"
					   "git branch experimental")
		    ,(make-open-problem "How can you tell what different branches exist as part of the same project?"
					   "git branch")
		    ,(make-open-problem "How would you switch to the 'experimental' branch in your project?"
					   "git checkout experimental")
		    ,(make-open-problem "How would you merge the changes
made in 'experimental' branch back into main branch?"
					   "git merge experimental")
		    ,(make-open-problem "How would you delete the 'experimental' branch of your project, ensuring that its changes have been committed to the 'master' branch?"
					   "git branch -d experimental")
		    ,(make-open-problem "How would you delete the 'crazy-idea' branch of your project, without adding it to the 'master' branch?"
					   "git branch -D crazy-idea"))))

(define git-gmodule
  (gmod_make-gmodule
   (id 'git)
   (name "Git: fast version control")
   (version "0.1")
   (description "Learn to use git to manage your projects.")
   (long-description "Long Description: background on git, introductory text")
   (creators "Alex Sassmannshausen")
   (derivation-source "Git man pages & website")
   (parts `(,git-init ,git-status ,git-add ,git-commit ,git-diff ,git-log ,git-branch))
   (find-out-more "http://www.git-scm.com")))

(gman_add-gmodule git-gmodule)

