#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

dnl Require at least autoconf version...
AC_PREREQ([2.69])

dnl      Official Name, Version, Bug-address, Tarball Name, Website
AC_INIT([Glean],
        [0.1],
        [alex.sassmannshausen@gmail.com],
        [glean],
        [www.glean.org])
AC_CONFIG_AUX_DIR([build-aux])

dnl Initiate automake, display all warnings
AM_INIT_AUTOMAKE([-Wall])
dnl Not sure about this one.
AC_REVISION([$Revision: 0.1 $])
dnl Verify glean exists in the directory
dnl that autoconf was pointetd to.
AC_CONFIG_SRCDIR([scripts/glean.in])

dnl Where to find guile modules.
guilemoduledir="${datarootdir}/guile/site/2.0"
AC_SUBST([guilemoduledir])

dnl Test installed Guile version.
PKG_CHECK_MODULES([GUILE], [guile-2.0 >= 2.0.5])
AC_PATH_PROG([GUILE], [guile])
AC_PATH_PROG([GUILD], [guild])
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

dnl Make sure we have a full-fledged Guile.
GUIX_ASSERT_GUILE_FEATURES([regex posix socket net-db threads])

dnl Check whether Artanis is installed; install otherwise.
dnl GLEAN_CHECK_ARTANIS
dnl AM_CONDITIONAL([INSTALL_ARTANIS],
dnl                [test "x$ac_cv_glean_artanis_installed" = xno])
dnl For now we consider the web client experimental and don't even
dnl check whether artanis is available.
dnl GUILE_MODULE_REQUIRED([artanis artanis])

dnl Locate libgcrypt & test it.
AC_ARG_WITH([libgcrypt-prefix],
  [AS_HELP_STRING([--with-libgcrypt-prefix=DIR], [search for GNU libgcrypt in DIR])],
  [case "$withval" in
    yes|no)
      LIBGCRYPT="libgcrypt"
      LIBGCRYPT_PREFIX="no"
      ;;
    *)
      LIBGCRYPT="$withval/lib/libgcrypt"
      LIBGCRYPT_PREFIX="$withval"
      ;;
   esac],
  [LIBGCRYPT="libgcrypt"])

dnl Library name suitable for `dynamic-link'.
AC_MSG_CHECKING([for libgcrypt shared library name])
AC_MSG_RESULT([$LIBGCRYPT])
AC_SUBST([LIBGCRYPT])
AC_SUBST([LIBGCRYPT_PREFIX])

GUIX_ASSERT_LIBGCRYPT_USABLE

dnl Outro
AC_CACHE_SAVE

dnl Make these files from their *.ins
dnl From Manual: "Automake uses these
dnl to determine which files to create"
AC_CONFIG_FILES([
  Makefile
  glean/config.scm
])

AC_CONFIG_FILES([scripts/glean], [chmod +x scripts/glean])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([test-env],     [chmod +x test-env])

AC_OUTPUT
