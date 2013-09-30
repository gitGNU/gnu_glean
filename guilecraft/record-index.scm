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

;;; Commentary:
;;
;; Module providing a list of dummy records for use with register-rtds
;;
;;; Code:

(define-module (guilecraft record-index)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft data-types requests)
  #:export (records))

(define records
  (list (request 'content)
	(response 'content)

	(alive-rq)
	(auth-rq 'id)
	(profs-rq)
	(quit-rq)

	(chall-rq 'profile)
	(chall-rs 'profile 'challenge)
	(eval-rq 'profile 'answer)
	(eval-rs 'profile 'eval-result)

	(ack-rs 'original)
	(neg-rs 'original)
	(auth-rs 'profile)
	(profs-rs 'list)
	(unk-rs 'original)
	
	(gmodule 
	 (id 'id)
	 (name 'name)
	 (version 'version)
	 (synopsis 'description)
	 (description 'long-description)
	 (creators 'creators)
	 (parts 'parts)
	 (find-out-more 'find-out-more)
	 (derivation-source 'get-derivation-source))
	
	(make-id 'name 'timestamp)
	
	(make-profile
	 (name 'name)
	 (id 'id)
	 (active-modules 'active-modules)
	 (scorecard 'scorecard))
	
	(make-gset 'tag 'problems)
	
	(make-gset-blob 'set-tag 'score 'counter)
	(make-gmod-blob 'gmodule-id 'gset-data)
	(make-scorecard 'data)))
