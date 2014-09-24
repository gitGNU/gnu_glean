;; lounge-requests.scm --- lounge server requests   -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
;;
;; This file is part of Glean.
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
;; This module defines the vocabulary for requests specifically used when
;; communicating with a lounge server.
;;
;; Currently communications occurs through these distinct data-types,
;; serialized to plain lists by functionality defined in comtools, and then
;; transformed back into the respective data-types at the other end of the
;; socket.
;;
;; In order to reduce the depth of the comms tower before and after the use of
;; a socket we may switch to using simple lists which can be destructured with
;; (ice-9 match) instead of distinct data-types at a future point.
;;
;; The benefits of this would be that:
;; - we can carry out all input parsing at the point where data is read from
;;   the socket…
;; - we can extend the vocabulary in a simpler way by not having to define a
;;   new record type here…
;; - we remove the need for a hairy rnrs->list and list->rnrs record
;;   conversion for each communication sent…
;; - conversion to and from json/xml might be easier to implement.
;;
;;; Code:

(define-module (glean common lounge-requests)
  #:use-module (rnrs records procedural)
  #:export (
            echoq
            echoq?
            echoq-token
            echoq-message
            echos
            echos?
            echos-token
            echos-lounge
            echos-library
            echos-message

            regq
	    regq?
	    regq-name
	    regq-password
	    regq-prof-server
	    regq-mod-server

            viewq
            viewq?
            viewq-token
            views
            views?
            views-token
            views-details

	    set!q
	    set!q?
	    set!q-token
	    set!q-field
	    set!q-value

	    set!s
	    set!s?
	    set!s-token
	    set!s-field
	    set!s-value

	    delq
	    delq?
	    delq-token

	    authq
	    authq?
	    authq-name
	    authq-password
	    auths
	    auths?
	    auths-token
	    auths-mod-server
	    auths-prof-server

	    chauthq
	    chauthq?
	    chauthq-token
	    chauths
	    chauths?
	    chauths-token
	    chauths-hash
	    chauths-counter

	    evauthq
	    evauthq?
	    evauthq-token
	    evauthq-result))


;;;;; Lounge Requests

;;;; Request Echo
;; Echo requests are a simple test request that returns a new token,
;; the profile-server, the module-server and the test-message supplied
;; to the requestor.
;; Returns an Echos.
(define echoq-rtd
  (make-record-type-descriptor 'echoq #f #f #f #f
			       '#((immutable token)
                                  (immutable message))))
(define echoq-rcd
  (make-record-constructor-descriptor echoq-rtd #f #f))
(define echoq (record-constructor echoq-rcd))
(define echoq? (record-predicate echoq-rtd))
(define echoq-token (record-accessor echoq-rtd 0))
(define echoq-message (record-accessor echoq-rtd 1))

;;;; Provide Echo
(define echos-rtd
  (make-record-type-descriptor 'echos #f #f #f #f
			       '#((immutable token)
				  (immutable prof-server)
				  (immutable mod-server)
                                  (immutable message))))
(define echos-rcd
  (make-record-constructor-descriptor echos-rtd #f #f))
(define echos (record-constructor echos-rcd))
(define echos? (record-predicate echos-rtd))
(define echos-token (record-accessor echos-rtd 0))
(define echos-lounge (record-accessor echos-rtd 1))
(define echos-library (record-accessor echos-rtd 2))
(define echos-message (record-accessor echos-rtd 3))


;;;; Request Registration
;; Reg requests provide a profile name and a module server (no
;; password for now) and are responded to with an auths if all went well, or
;; negs otherwise.
(define regq-rtd
  (make-record-type-descriptor 'regq #f #f #f #f
			       '#((immutable name)
                                  (immutable password)
				  (immutable prof-server)
				  (immutable mod-server))))
(define regq-rcd
  (make-record-constructor-descriptor regq-rtd #f #f))
(define regq (record-constructor regq-rcd))
(define regq? (record-predicate regq-rtd))
(define regq-name (record-accessor regq-rtd 0))
(define regq-password (record-accessor regq-rtd 1))
(define regq-prof-server (record-accessor regq-rtd 2))
(define regq-mod-server (record-accessor regq-rtd 3))

;;;; Request View
;; View requests provide a means to retriew profile details from the
;; lounge. They should contain a valid token. They are responded to
;; with a view response.
;; Returns a views, or negs on error.
(define viewq-rtd
  (make-record-type-descriptor 'viewq #f #f #f #f
			       '#((immutable token))))
(define viewq-rcd
  (make-record-constructor-descriptor viewq-rtd #f #f))
(define viewq (record-constructor viewq-rcd))
(define viewq? (record-predicate viewq-rtd))
(define viewq-token (record-accessor viewq-rtd 0))

;;;; Provide View
(define views-rtd
  (make-record-type-descriptor 'views #f #f #f #f
			       '#((immutable token)
                                  (immutable details))))
(define views-rcd
  (make-record-constructor-descriptor views-rtd #f #f))
(define views (record-constructor views-rcd))
(define views? (record-predicate views-rtd))
(define views-token (record-accessor views-rtd 0))
(define views-details (record-accessor views-rtd 1))


;;;; Request Set!
;; Set! Requests provide a token, a symbol identifier of the field
;; indicated for modification (e.g. 'active-modules, 'name,
;; 'prof-server, etc.) and an appropriate new value for that field.
;; If no further action is required, return auths, else set!s.
(define set!q-rtd
  (make-record-type-descriptor 'set!q #f #f #f #f
			       '#((immutable token)
				  (immutable field)
				  (immutable value))))
(define set!q-rcd
  (make-record-constructor-descriptor set!q-rtd #f #f))
(define set!q (record-constructor set!q-rcd))
(define set!q? (record-predicate set!q-rtd))
(define set!q-token (record-accessor set!q-rtd 0))
(define set!q-field (record-accessor set!q-rtd 1))
(define set!q-value (record-accessor set!q-rtd 2))

;;;; Provide Set! Incomplete
;; Indicate to the client that further action is required.
(define set!s-rtd
  (make-record-type-descriptor 'set!s #f #f #f #f
			       '#((immutable token)
				  (immutable field)
				  (immutable value))))
(define set!s-rcd
  (make-record-constructor-descriptor set!s-rtd #f #f))
(define set!s (record-constructor set!s-rcd))
(define set!s? (record-predicate set!s-rtd))
(define set!s-token (record-accessor set!s-rtd 0))
(define set!s-field (record-accessor set!s-rtd 1))
(define set!s-value (record-accessor set!s-rtd 2))


;;;; Request Authentication
;; Auth requests provide a profile name (no password for now) and are
;; responded to with a auth-response.
;; Returns an auths, or negs on error.
(define authq-rtd
  (make-record-type-descriptor 'authq #f #f #f #f
			       '#((immutable name)
                                  (immutable password))))
(define authq-rcd
  (make-record-constructor-descriptor authq-rtd #f #f))
(define authq (record-constructor authq-rcd))
(define authq? (record-predicate authq-rtd))
(define authq-name (record-accessor authq-rtd 0))
(define authq-password (record-accessor authq-rtd 1))

;;;; Provide Authentication
;; Auth responses provide a token to be used by the client for future
;; requests.
;; Auths are used as a standard 'success' response for a variety of lounge
;; requests as further requests will normally require an authentication
;; token, as provided by auths.
(define auths-rtd
  (make-record-type-descriptor 'auths #f #f #f #f
			       '#((immutable token)
                                  (immutable prof-server)
				  (immutable mod-server))))
(define auths-rcd
  (make-record-constructor-descriptor auths-rtd #f #f))
(define auths (record-constructor auths-rcd))
(define auths? (record-predicate auths-rtd))
(define auths-token (record-accessor auths-rtd 0))
(define auths-prof-server (record-accessor auths-rtd 1))
(define auths-mod-server (record-accessor auths-rtd 2))


;;;; Request Challenge ID
;; Chauthq requires a token and responds normally with chauths, or a negs on
;; error.
(define chauthq-rtd
  (make-record-type-descriptor 'chauthq #f #f #f #f
			       '#((immutable token))))
(define chauthq-rcd
  (make-record-constructor-descriptor chauthq-rtd #f #f))
(define chauthq (record-constructor chauthq-rcd))
(define chauthq? (record-predicate chauthq-rtd))
(define chauthq-token (record-accessor chauthq-rtd 0))

;;;; Provide Challenge ID
;; Chauths provide the blobhash and counter of the next challenge to
;; be asked, as well as the mod-server to be used by the client.
(define chauths-rtd
  (make-record-type-descriptor 'chauths #f #f #f #f
			       '#((immutable token)
				  (immutable hash)
				  (immutable counter))))
(define chauths-rcd
  (make-record-constructor-descriptor chauths-rtd #f #f))
(define chauths (record-constructor chauths-rcd))
(define chauths? (record-predicate chauths-rtd))
(define chauths-token (record-accessor chauths-rtd 0))
(define chauths-hash (record-accessor chauths-rtd 1))
(define chauths-counter (record-accessor chauths-rtd 2))

;;;; Request Evaluation Profile Update
;; Evauth requests provide a token originally furnished in the most
;; recent auth request, to be used by the prof-server to identify the
;; scorecard to update in light of the result.
;; Returns an auths, or negs on error.
(define evauthq-rtd
  (make-record-type-descriptor 'evauthq #f #f #f #f
			       '#((immutable token)
				  (immutable result))))
(define evauthq-rcd
  (make-record-constructor-descriptor evauthq-rtd #f #f))
(define evauthq (record-constructor evauthq-rcd))
(define evauthq? (record-predicate evauthq-rtd))
(define evauthq-token (record-accessor evauthq-rtd 0))
(define evauthq-result (record-accessor evauthq-rtd 1))

;;;; Request Profile Deletion
;; Del Requests provide a token to identify the player profile to be
;; deleted.
;; Returns an acks on success, a negs otherwise.
(define delq-rtd
  (make-record-type-descriptor 'delq #f #f #f #f
			       '#((immutable token))))
(define delq-rcd
  (make-record-constructor-descriptor delq-rtd #f #f))
(define delq (record-constructor delq-rcd))
(define delq? (record-predicate delq-rtd))
(define delq-token (record-accessor delq-rtd 0))

;;; lounge-requests.scm ends here
