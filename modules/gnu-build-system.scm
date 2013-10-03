;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (modules gnu-build-system)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft problem-types open-problems)
  #:use-module (guilecraft problem-types multi-choice-problems)
  #:export (gbs-gmodule))

(define gbs-prefix
  (make-gset 'gbs-prefix
		  `(,(make-open-problem "Name the variable defining
the overall prefix for the installation"
					   "prefix")
		    ,(make-multi-choice-problem "The variable defining
the first part of the installation's target path is:"
						(cons "a" "prefix")
						(cons "a" "prefix")
						(cons "b" "exec_prefix")
						(cons "c" "target"))
		    ,(make-multi-choice-problem "Given the path
'/usr/local/man', what is the prefix part used by default?"
						(cons "b" "/usr/local")
						(cons "a" "/usr")
						(cons "b"
						      "/usr/local")
						(cons "c" "It is not
present")))))

(define gbs-prefix
  (make-gset 'gbs-prefix
		  `(,(make-open-problem "Name the variable defining
the overall prefix for the installation"
					   "prefix")
		    ,(make-multi-choice-problem "The variable defining
the first part of the installation's target path is:"
						(cons "a" "prefix")
						(cons "a" "prefix")
						(cons "b" "exec_prefix")
						(cons "c" "target"))
		    ,(make-multi-choice-problem "Given the path
'/usr/local/man', what is the prefix part used by default?"
						(cons "b" "/usr/local")
						(cons "a" "/usr")
						(cons "b"
						      "/usr/local")
						(cons "c" "It is not
present")))))

(define gbs-gmodule
  (gmodule
   (id 'gnu-build-system)
   (name "The GNU Build System")
   (version "0.1")
   (synopsis "Fundamentals of the GNU Build System")
   (description "It is a truth universally acknowledged, that as a
developer in possession of a new package, you must be in want of a
build system.

   In the Unix world, such a build system is traditionally achieved
using the command 'make' (*note Overview: (make)Top.).  You express
the recipe to build your package in a 'Makefile'.  This file is a set
of rules to build the files in the package.  For instance the program
'prog' may be built by running the linker on the files 'main.o',
'foo.o', and 'bar.o'; the file 'main.o' may be built by running the
compiler on 'main.c'; etc.  Each time 'make' is run, it reads
'Makefile', checks the existence and modification time of the files
mentioned, decides what files need to be built (or rebuilt), and runs
the associated commands.

   When a package needs to be built on a different platform than the
one it was developed on, its 'Makefile' usually needs to be adjusted.
For instance the compiler may have another name or require more
options.  In 1991, David J. MacKenzie got tired of customizing
'Makefile' for the 20 platforms he had to deal with.  Instead, he
handcrafted a little shell script called 'configure' to automatically
adjust the 'Makefile' (*note Genesis: (autoconf)Genesis.).  Compiling
his package was now as simple as running './configure && make'.

   Today this process has been standardized in the GNU project.  The
GNU Coding Standards (*note The Release Process: (standards)Managing
Releases.) explains how each package of the GNU project should have a
'configure' script, and the minimal interface it should have.  The
'Makefile' too should follow some established conventions.  The
result?  A unified build system that makes all packages almost
indistinguishable by the installer.  In its simplest scenario, all the
installer has to do is to unpack the package, run './configure && make
&& make install', and repeat with the next package to install.

   We call this build system the \"GNU Build System\", since it was
grown out of the GNU project.  However it is used by a vast number of
other packages: following any existing convention has its advantages.

   The Autotools are tools that will create a GNU Build System for your
package.  Autoconf mostly focuses on 'configure' and Automake on
'Makefile's.  It is entirely possible to create a GNU Build System
without the help of these tools.  However it is rather burdensome and
error-prone.  We will discuss this again after some illustration of the
GNU Build System in action.
-- (Taken from the auomake manual)")
   (creators "Alex Sassmannshausen")
   (derivation-source "Autotools Manuals")
   (parts `(,prefix ,exec_prefix ,bindir ,libdir ,includedir
		    ,datarootdir ,datadir ,mandir ,infodir ,docdir))
   (find-out-more "http://www.gnu.org/prep/standards/")))
