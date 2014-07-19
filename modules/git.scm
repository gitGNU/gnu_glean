;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (store git)
  #:use-module (guilecraft core-templates)
  #:export (git-module))

(define git-init
  (set 'git-init
	#:contents
	(list
	 (problem (q "What git command would you use to initiate a new project in the current directory?")
		  (s "git init"))
	 (problem (q "In order to create a new git repository, would you:")
		  (s "type git init")
		  (o "type git test")
		  (o "type git init")
		  (o "type rm -r *")))))
(define git-status
  (set 'git-status
	#:contents
	(list
	 (problem (q "How would you get a quick overview of the current status of the project in your current directory?")
		  (s "git status")))))
(define git-add
  (set 'git-add
	#:contents
	(list
	 (problem (q "How would you add all newly created files & folders to the index of your project in the current directory?")
		  (s "git add ."))
	 (problem (q "How would you add foo to the index of your project in the current directory?")
		  (s "git add foo"))
	 (problem (q "How would you add foo/bar to index of your project in the current directory?")
		  (s "git add foo/bar")))))
;; Many revision control systems provide an add command that tells the system to start
;; tracking changes to a new file. Git’s add command does something simpler and more
;; powerful: git add is used both for new and newly modified files, and in both cases it
;; takes a snapshot of the given files and stages that content in the index, ready for
;; inclusion in the next commit.

(define git-commit
  (set 'git-commit
	#:contents
	(list
	 (problem (q "How would you commit your changes to the git
repository for your current project?")
		  (s "git commit"))
	 (problem (q "What is a git shortcut for both adding your
recent changes to the index and committing the index to the
repository? (this will not add any newly createy files in the project
structure to the repository!)")
		  (s "git commit -a")))))

;; A note on commit messages: Though not required, it's a good idea to
;; begin the commit message with a single short (less than 50 character)
;; line summarizing the change, followed by a blank line and then a more
;; thorough description. Tools that turn commits into email, for example,
;; use the first line on the Subject: lin and the rest of the commit in
;; the body.

(define git-diff
  (set 'git-diff
	#:contents
	(list
	 (problem (q "How can you tell what you are ready to commit to
your git repository in the current directory?")
		  (s "git diff --cached"))
	 (problem (q "How can you tell what changes have been made
since your last commit/add?")
		  (s "git diff")))))

(define git-log
  (set 'git-log
	#:contents
	(list
	 (problem (q "How can you view the history of your changes?")
		  (s "git log"))
	 (problem (q "How can you vie the complete diffs at each step
of your history?")
		  (s "git log -p"))
	 (problem (q "How can you get an overview of the changes of
each step?")
		  (s "git log --stat --summary")))))

(define git-branch
  (set 'git-branch
	#:contents
	(list
	 (problem (q "How would you branch your current project to
create a new branch named 'experimental'?")
		  (s "git branch experimental"))
	 (problem (q "How can you tell what different branches exist as
part of the same project?")
		  (s "git branch"))
	 (problem (q "How would you switch to the 'experimental' branch
in your project?")
		  (s "git checkout experimental"))
	 (problem (q "How would you merge the changes
made in 'experimental' branch back into main branch?")
		  (s "git merge experimental"))
	 (problem (q "How would you delete the 'experimental' branch of
your project, ensuring that its changes have been committed to the
'master' branch?")
		  (s "git branch -d experimental"))
	 (problem (q "How would you delete the 'crazy-idea' branch of
your project, without adding it to the 'master' branch?")
		  (s "git branch -D crazy-idea")))))

(define git-checkout
  (set 'git-checkout
	#:contents
	(list
	 (problem (q "How would you switch to an existing branch
called 'experimental'?")
		  (s "git checkout experimental"))
	 (problem (q "How would you create a new branch, 'feature',
and switch to it?")
		  (s "git checkout -b feature")))))

(define git-module
  (module
    'git
    #:name "Git: fast version control"
    #:version "0.1"
    #:keywords '("programming" "version-control" "project-management"
                "source-code")
    #:synopsis "Learn to use git to manage your projects."
    #:description "Long Description: background on git, introductory text"
    #:creator "Alex Sassmannshausen"
    #:attribution (list (media #:text "Git man pages & website"))
    #:contents `(,git-init ,git-status ,git-add ,git-commit ,git-diff
			   ,git-log ,git-branch ,git-checkout)
    #:logo "http://www.git-scm.com/images/logo@2x.png"
    #:resources (list (media #:urls '("http://www.git-scm.com")))))
