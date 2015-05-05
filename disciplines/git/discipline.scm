;; discipline.scm --- the git discipline -*- coding: utf-8 -*-
;;
;; This file is part of Glean.
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
;;
;; Glean is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; Glean is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with glean; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; This module defines the git discipline.
;;
;;; Code:

(define-module
  (glean disciplines git discipline)
  #:use-module
  (glean disciplines git ancestry)
  #:use-module
  (glean library core-templates)
  #:export
  (git))


;;;; Exercises
;;;
;;; For clarity we define the exercises at the top level.  As a result we
;;; define the discipline itself at the very bottom of the file.

(define init
  (set 'init
       #:contents
       (list
        (problem (q "What Git command would you use to initiate a new project
in the current directory?")
                 (s "git init"))
        (problem (q "In order to create a new Git repository, would you type:")
                 (s "git init")
                 (o "git start")
                 (o "git init")
                 (o "git create"))
        (problem (q "What is the first command to issue when you want to
start using Git for a project?")
                 (s "git init"))
        (problem (q "What would the command ‘git init‘ do?")
                 (s "Create a new git project in the current directory.")
                 (o "Create a new git project in the current directory.")
                 (o "Check the status of the current git project.")
                 (o "Show an overview of recent actions.")))))

(define status
  (set 'status
       #:contents
       (list
        (problem (q "In Git, how would you get an overview of the current
branch of the project in your working directory?")
                 (s "git status"))
        (problem (q "To get a quick overview of the Git project in your
current directory, you would type ‘git …‘")
                 (s "status")
                 (o "init")
                 (o "status")
                 (o "log"))
        (problem (q "What command will list files that have been changed
since your last commit?")
                 (s "git status"))
        (problem (q "’git status’ will show you:")
                 (s "an overview of your current git branch.")
                 (o "an overview of your current git branch.")
                 (o "information about your latest commit.")
                 (o "a detailed set of change logs since for the files you
have changed since your last commit..")))))

(define add
  (set 'add
        #:contents
        (list
         (problem (q "How would you add ’foo’ to the index of your project in
the current directory?")
                  (s "git add foo"))
         (problem (q "You have just changed a file, and you want to make sure
that the file, as it exists now is commited with your next comit. What command
would you use?")
                  (s "git add")
                  (o "git diff")
                  (o "git commit")
                  (o "git add"))
         (problem (q "How would you add ‘foo/bar‘ to index of your project in
the current directory?")
                  (s "git add foo/bar"))
         (problem (q "To start tracking ‘foo’ with Git, what command would you use?")
                  (s "git add")
                  (o "git add")
                  (o "git init")
                  (o "git track")))))

(define commit
  (set 'commit
       #:contents
       (list
        (problem (q "‘true’ or ’false’: you can commit the changes you have
added to your Git index using the ’git commit‘ command.")
                 (s "true")
                 (o "true")
                 (o "false"))
        (problem (q "After having used the ‘git add‘ command, what command
would you use commit your changes to the repository?")
                 (s "git commit"))
        (problem (q "Will ’git commit‘ commit changes to files in your
repository even if those changes have not been added to the Git index?")
                 (s "no")
                 (o "yes")
                 (o "no"))
        (problem (q "Making changes to your files permanent in your
repository is done by using…")
                 (s "git commit")
                 (o "git commit")
                 (o "git add")
                 (o "git log")))))

(define diff
  (set 'diff
       #:contents
       (list
        (problem (q "Which of the following commands would you use to obtain
a detailed report on your work since your last commit?")
                 (s "git diff")
                 (o "git show")
                 (o "git status")
                 (o "git diff"))
        (problem (q "How can you tell what changes have been made since your
last commit?")
                 (s "git diff"))
        (problem (q "What command would you use to view a diff-style report
on the files tracked in your project?")
                 (s "git diff")))))

(define log
  (set 'log
       #:contents
       (list
        (problem (q "‘true‘ or ‘false‘: ‘git log’ allows you to see a snapshot of
the current status of your project")
                 (s "false")
                 (o "true")
                 (o "false"))
        (problem (q "How can you view the history of your commits?")
                 (s "git log"))
        (problem (q "The command ’git log’ allows you to…")
                 (s "view your commit history.")
                 (o "view your commit history.")
                 (o "write a commit message.")
                 (o "view your last commit.")))))

(define branch
  (set 'branch
       #:contents
       (list
        (problem (q "To list the currently existing branches in your project,
you would type:")
                 (s "git branch")
                 (o "git status")
                 (o "git checkpoints")
                 (o "git branch"))
        (problem (q "How would you create a new branch named ‘experimental‘
in your current project?")
                 (s "git branch experimental"))
        (problem (q "How can you tell what different branches exist as
part of the same project?")
                 (s "git branch"))
        (problem (q "To create a branch called ’feature’, you'd type…")
                 (s "git branch feature")
                 (o "git target feature")
                 (o "git branch feature")
                 (o "git checkout feature")))))

(define checkout
  (set 'checkout
       #:contents
       (list
        (problem (q "Switching to the ‘feature’ branch is done by typing…")
                 (s "git checkout feature")
                 (o "git check feature")
                 (o "git checkout feature")
                 (o "git branch feature"))
        (problem (q "How would you switch to an existing branch
called ‘experimental’?")
                 (s "git checkout experimental"))
        (problem (q "Can you switch to a different branch whilst you have
uncommited, but tracked, files in your repository?")
                 (s "no")
                 (o "yes")
                 (o "no")))))


;;;; Discipline
;;;
;;; We have defined the exercises above, so we now we can simply define the
;;; discipline's meta data

(define git
  (module
      'git
      #:name "Git: fast version control"
      #:version "0.1"
      #:keywords '("programming" "version-control" "project-management"
                   "source-code")
      #:synopsis "Learn to use git to manage your projects."
      #:description "Git is a free and open source distributed version control
system designed to handle everything from small to very large projects with
speed and efficiency."
      #:creator "Alex Sassmannshausen"
      #:ancestry (ancestry-trees)
      #:attribution (list (media #:text "Git man pages & website"))
      #:contents `(,init ,status ,add ,commit ,diff ,log ,branch
                         ,checkout)
      #:logo "http://www.git-scm.com/images/logo@2x.png"
      #:resources (list (media #:urls '("http://www.git-scm.com")))))

;;; discipline.scm ends here
