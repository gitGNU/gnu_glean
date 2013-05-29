;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;; Copyright (C) 2008, 2010, 2012 Alex Sassmannshausen

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(define-module (tests portal)
  #:use-module (srfi srfi-64)
  #:use-module (tests test-utils)

  #:use-module (guilecraft portal)
  #:use-module (guilecraft gmodule-manager)
  #:use-module (guilecraft data-types gprofiles))

(gman_add-gmodule test-gmodule)

(test-begin "portal-tests")

(test-assert "challenge request"
  (gprof_profile?
   (car
    (port_portal
     (port_make-challenge-request
      test-profile)))))

(test-assert "eval request"
  (gprof_profile?
   (car
    (port_portal
     (port_make-eval-request
      "solution"
      test-profile)))))

(test-end "portal-tests")