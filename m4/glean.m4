dnl GNU Guix --- Functional package management for GNU
dnl Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
dnl
dnl This file is part of GNU Guix.
dnl
dnl GNU Guix is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl GNU Guix is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

# serial 1

# # GLEAN_CHECK_ARTANIS
# # dnl
# # dnl Check whether Nala Ginrut's Artanis has already been installed.
AC_DEFUN([GLEAN_CHECK_ARTANIS], [
  AC_CACHE_CHECK([whether (artanis artanis) is already installed],
    [ac_cv_glean_artanis_installed],
    [if "$GUILE" -c "(use-modules (artanis artanis))"
     then
       ac_cv_glean_artanis_installed=yes
     else
       ac_cv_glean_artanis_installed=no
     fi])
])
