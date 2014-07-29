# glean --- fast learning tool.         -*- coding: utf-8 -*-
#
# Copyright © 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
#
# This file is part of Glean.
#
# Glean is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# Glean is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, contact:
#
# Free Software Foundation           Voice:  +1-617-542-5942
# 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
# Boston, MA  02111-1307,  USA       gnu@gnu.org

bin_SCRIPTS =						\
  scripts/glean

nodist_noinst_SCRIPTS =					\
  pre-inst-env

MODULES =						\
  glean/base32.scm				     	\
  glean/base-server.scm					\
  glean/boot.scm					\
  glean/client/boot.scm					\
  glean/clients/cli.scm					\
  glean/clients/monadic-min.scm				\
  glean/clients/repl-client-core.scm	     		\
  glean/clients/web/web-client-core.scm			\
  glean/clients/web/bootstrap.scm		  	\
  glean/components.scm				   	\
  glean/comtools.scm				   	\
  glean/config-utils.scm			   	\
  glean/core-templates.scm			   	\
  glean/data-manager.scm			     	\
  glean/data-types/base-requests.scm	     		\
  glean/data-types/gprofiles.scm		   	\
  glean/data-types/module-requests.scm	   		\
  glean/data-types/profile-requests.scm			\
  glean/data-types/scorecards.scm		  	\
  glean/data-types/sets.scm			  	\
  glean/hash.scm					\
  glean/known-rtd-manager.scm				\
  glean/library/boot.scm			     	\
  glean/library-store.scm				\
  glean/lounge/boot.scm					\
  glean/lounge-filesystem.scm				\
  glean/lounge-store.scm			     	\
  glean/module-server.scm			    	\
  glean/monads.scm				     	\
  glean/profile-server.scm			   	\
  glean/record-index.scm			     	\
  glean/utils.scm					\
  glean.scm

GOBJECTS = $(MODULES:%.scm=%.go) glean/config.go

nobase_dist_guilemodule_DATA = $(MODULES)
nobase_nodist_guilemodule_DATA = $(GOBJECTS) glean/config.scm

# Handy way to remove the .go files without removing all the rest.
clean-go:
	-$(RM) -f $(GOBJECTS)

SCM_TESTS =						\
  tests/sets.scm					\
  tests/scorecards.scm				 	\
  tests/base32.scm				     	\
  tests/hash.scm					\
  tests/lounge-store.scm			     	\
  tests/lounge-server.scm			    	\
  tests/library-store.scm			    	\
  tests/library-server.scm			   	\
  tests/comtools-offline.scm

TESTS = $(SCM_TESTS)

TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

SH_LOG_COMPILER = $(top_builddir)/test-env $(SHELL)
AM_SH_LOG_FLAGS = -x -e

EXTRA_DIST =					     	\
  artanis						\
  www							\
  modules						\
  HACKING						\
  ROADMAP						\
  TODO						     	\
  .dir-locals.el					\
  srfi/srfi-64.scm				     	\
  srfi/srfi-64.upstream.scm			  	\
  quickcheck/quickcheck.scm			  	\
  tests/quickcheck-defs.scm			  	\
  tests/test-utils.scm				 	\
  tests/test-suite.scm				 	\
  $(TESTS)

CLEANFILES =						\
  $(GOBJECTS)						\
  $(SCM_TESTS:tests/%.scm=%.log)

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC" $@;

# XXX: Use the C locale for when Guile lacks
# <http://git.sv.gnu.org/cgit/guile.git/commit/?h=stable-2.0&id=e2c6bf3866d1186c60bacfbd4fe5037087ee5e3f>.
.scm.go:
	$(AM_V_GUILEC)$(MKDIR_P) `dirname "$@"` ;			\
	LC_ALL=C							\
	$(top_builddir)/pre-inst-env					\
	$(GUILD) compile -L "$(top_builddir)" -L "$(top_srcdir)"	\
	  -Wformat -Wunbound-variable -Warity-mismatch			\
	  -o "$@" "$<"

SUFFIXES = .go

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guix_install_go_files = install-nobase_nodist_guilemoduleDATA
$(guix_install_go_files): install-nobase_dist_guilemoduleDATA

#SUBDIRS = po

include doc.am

ACLOCAL_AMFLAGS = -I m4
AM_DISTCHECK_CONFIGURE_FLAGS =						\
  --with-libgcrypt-prefix="$(LIBGCRYPT_PREFIX)"

dist-hook: gen-ChangeLog

gen-ChangeLog:
	if test -d .git; then						\
	  $(top_srcdir)/build-aux/gitlog-to-changelog			\
	    > $(distdir)/cl-t;						\
	  rm -f $(distdir)/ChangeLog;					\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;			\
	fi

.PHONY: gen-ChangeLog clean-go

# Remove git from artanis distribution.
dist-hook:
	find $(distdir)/artanis -name ".git*" -delete
