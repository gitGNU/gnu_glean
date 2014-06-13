# guilecraft --- fast learning tool.         -*- coding: utf-8 -*-
#
# Copyright © 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
#
# This file is part of Guilecraft.
#
# Guilecraft is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# Guilecraft is distributed in the hope that it will be useful,
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

# FIXME: will be used once I'm using diverse launcher scripts.
# bin_SCRIPTS =					\
#   scripts/guix

nodist_noinst_SCRIPTS =				\
  pre-inst-env

MODULES =					\
  guilecraft/base32.scm				\
  guilecraft/base-server.scm			\
  guilecraft/boot.scm				\
  guilecraft/clients/cli.scm			\
  guilecraft/clients/monadic-min.scm		\
  guilecraft/clients/repl.scm			\
  guilecraft/clients/web/web.scm		\
  guilecraft/clients/web/bootstrap.scm		\
  guilecraft/comtools.scm			\
  guilecraft/data-manager.scm			\
  guilecraft/data-types/base-requests.scm	\
  guilecraft/data-types/gprofiles.scm		\
  guilecraft/data-types/module-requests.scm	\
  guilecraft/data-types/profile-requests.scm	\
  guilecraft/data-types/scorecards.scm		\
  guilecraft/data-types/sets.scm		\
  guilecraft/hash.scm				\
  guilecraft/known-rtd-manager.scm		\
  guilecraft/library-store.scm			\
  guilecraft/lounge-filesystem.scm		\
  guilecraft/lounge-store.scm			\
  guilecraft/module-server.scm			\
  guilecraft/monads.scm				\
  guilecraft/profile-server.scm			\
  guilecraft/record-index.scm			\
  guilecraft/utils.scm				\
  guilecraft.scm

GOBJECTS = $(MODULES:%.scm=%.go) guilecraft/config.go

nobase_dist_guilemodule_DATA = $(MODULES)
nobase_nodist_guilemodule_DATA = $(GOBJECTS) guilecraft/config.scm

# Handy way to remove the .go files without removing all the rest.
clean-go:
	-$(RM) -f $(GOBJECTS)

SCM_TESTS =					\
  tests/sets.scm				\
  tests/scorecards.scm				\
  tests/base32.scm				\
  tests/hash.scm				\
  tests/lounge-store.scm			\
  tests/lounge-server.scm			\
  tests/library-store.scm			\
  tests/library-server.scm			\
  tests/comtools-offline.scm

# SH_TESTS =					\
#   tests/guix-build.sh

TESTS = $(SCM_TESTS)

TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

SH_LOG_COMPILER = $(top_builddir)/test-env $(SHELL)
AM_SH_LOG_FLAGS = -x -e

EXTRA_DIST =					\
  artanis					\
  www						\
  modules					\
  HACKING					\
  ROADMAP					\
  TODO						\
  .dir-locals.el				\
  srfi/srfi-64.scm				\
  srfi/srfi-64.upstream.scm			\
  quickcheck/quickcheck.scm			\
  quickcheck/quickcheck.sls			\
  tests/quickcheck-defs.scm			\
  tests/test-utils.scm				\
  tests/test-suite.scm				\
  $(TESTS)

CLEANFILES =					\
  $(GOBJECTS)					\
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
AM_DISTCHECK_CONFIGURE_FLAGS =			\
  --with-libgcrypt-prefix="$(LIBGCRYPT_PREFIX)"

dist-hook: gen-ChangeLog

gen-ChangeLog:
	if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    > $(distdir)/cl-t;				\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

.PHONY: gen-ChangeLog clean-go

# Remove git from artanis distribution.
dist-hook:
	find $(distdir)/artanis -name ".git*" -delete
