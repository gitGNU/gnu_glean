;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages glean)
  #:use-module ((guix licenses)
                #:select (gpl3+))
  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

;;; Commentary:
;;;
;;; A package definition for Glean.
;;;
;;; Code:

(define-public glean
  (package
    (name "glean")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.glean.org/glean-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1ja518vhjzbz8drmdg4n94j487ivk03l4dsyyxrkrzx776vrl91r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--with-libgcrypt-prefix="
                                              (assoc-ref %build-inputs
                                                         "libgcrypt")))))
    (inputs `(("libgcrypt" ,libgcrypt)
              ("guile" ,guile-2.0)
              ("pkg-config" ,pkg-config)))
    (synopsis "Glean test package.")
    (description
     "Glean is a...")
    (home-page "http://glean.org")
    (license gpl3+)))

;;; glean.scm ends here
