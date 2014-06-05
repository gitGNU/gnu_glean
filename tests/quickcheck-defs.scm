;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

;;;; Guilecraft Quickcheck Definitions

;;; copyright etc. & license.

;;; Commentary:

;;; Quickcheck is a property testing framework originally developed
;;; for Haskell. Guilecraft uses the portable scheme implementation of
;;; Quickcheck created by IJP.
;;;
;;; This library defines generators specific to guilecraft data-types
;;; for use by quickcheck tests.

;;;; Documentation
;;;  (Incomplete as this may still change)
;;;

;;; Code:

;;;;; Module Definition
(define-module (tests quickcheck-defs)
  #:use-module (rnrs)
  #:use-module (quickcheck quickcheck)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-27)
  #:use-module (guilecraft comtools)
  #:use-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types sets)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft data-types base-requests)
  #:use-module (guilecraft data-types module-requests)
  #:use-module (guilecraft data-types profile-requests)
  #:export (
	    $mk-rootset
            $mk-set
	    $question
	    $solution
	    $option
	    $medii
	    $prob
	    $pred

	    $profile
	    $id

	    $scorecard
	    $empty-scorecard
 	    $blob

	    $request
	    $response

	    $alive-rq
	    ;;$auth-rq
	    ;;$auth-rs
	    $chall-rq
	    $chall-rs
	    $eval-rq
	    $eval-rs
	    $ack-rs
	    $neg-rs
	    $unk-rs

	    $quit-rq

	    $datum
	    $record
	    $simple-tagged-list
            $tagged-list
	    quickname
            $short-list
	    $short-assoc
	    ))

;;;;; Set Generators
(define ($mk-rootset)
  "Return a randomised set."
  (make-set ($symbol) (($short-list $prob)) ; id / contents
            ($string) ($string)             ; name / version
            ($string) ($string)             ; synopsis / description
            (($short-list $string))         ; keywords
            ($string)                       ; creator
            (($short-list $medii))          ; attribution
            (($short-list $medii))          ; resources
            ($string)                       ; logo
            (($short-assoc $symbol $string)))) ; properties
(define ($mk-set)
  "Return a randomised set."
  (make-set ($symbol)                   ; id
            (($short-list $mk-rootset)) ; contents
            ($string) ($string)         ; name / version
            ($string) ($string)         ; synopsis / description
            (($short-list $string))     ; keywords
            ($string)                   ; creator
            (($short-list $medii))      ; attribution
            (($short-list $medii))      ; resources
            ($string)                   ; logo
            (($short-assoc $symbol $string)))) ; properties

;; FIXME: should randomly populate the media fields
(define ($medii)
  "Return a randomised media record."
  (media #:text ($string)))

(define ($prob)
  "Return a randomised problem."
  (apply problem ($question) ($solution) ($pred)
	 (($short-list $option))))
;; FIXME: Should include $medii at random points
(define ($question)
  "Return a randomised question."
  (q ($string)))
(define ($solution)
  "Return a randomised solution."
  (s ($string)))
(define ($option)
  "Return a randomised option."
  (o ($string)))
(define ($pred)
  "Return a randomised predicate."
  (p (from-list (list eq? = eqv? equal?))))
;;

;;;;; Profile Generators
;; FIXME: There should be a good chance that active-module set-ids
;; coincide with scorecard mod-blobs.
(define ($profile)
  "Return a randomised profile."
  (let ((name ($string)))
    (make-profile name ($id name) ($string) ($string)
                  (($short-list ($pair $symbol $symbol)))
                  ($scorecard))))

(define ($id name)
  "Return a randomised id."
  (make-id (string->symbol name) ($symbol)))

(define ($scorecard)
  "Return a randomised scorecard."
  (make-scorecard (($short-list $blob))))
(define ($empty-scorecard)
  "Return an empty scorecard."
  (make-scorecard '()))

(define ($blob)
  "Return a randomised mod-blob."
  (make-blob ($symbol) (list ($symbol)) (($short-list $symbol))
             ($small) ($integer) (($short-assoc $symbol $string))
             (($short-assoc $symbol $string))))

;;;;; Request Generators
(define ($unk-rs)
  "Return a randomised unk-response."
  (unk-rs ($datum)))
(define ($neg-rs)
  "Return a randomised neg-response."
  (neg-rs ($datum) ($symbol)))
(define ($ack-rs)
  "Return a randomised ack-response."
  (ack-rs ($datum)))

(define ($alive-rq)
  "Return an $alive-rq."
  (alive-rq))

(define ($eval-rq)
  "Return a randomised eval-request."
  (eval-rq ($profile)))
(define ($eval-rs)
  "Return a randomised eval-response."
  (eval-rs ($profile) ($boolean)))
(define ($chall-rq)
  "Return a randomised chall-request."
  (chall-rq ($profile)))
(define ($chall-rs)
  "Return a randomised chall-response."
  (chall-rs ($profile) ($challenge)))

(define ($quit-rq)
  "Return a quit-request."
  (quit-rq))

;;;;; Extra Generators & Functions
(define ($short-list generator)
  "Return a list with up to ten members of type returned by
GENERATOR."
  (lambda ()
    (build-list ($small)
		(lambda (_) (generator)))))
(define ($short-assoc key-generator value-generator)
  "Return an association list with up to ten members of type returned by
KEY-GENERATOR and VALUE-GENERATOR"
  (lambda ()
    (($short-list ($pair key-generator value-generator)))))

(define (quickname name)
  "Return is undefined. Print quickcheck intro message and NAME."
  (simple-format #t "starting quickcheck: ~a" name)
  (newline))

;; FIXME: In addition, should randomly return: request, record as
;; original
(define ($datum)
  "Return a random instance of one of the data enumerated in the list."
  ((from-list (list $string $symbol $integer $profile))))

(define ($record)
  "Return a random record enumerated in the list."
  ((from-list (list $profile $scorecard $mk-set))))

(define ($simple-tagged-list)
  "Return a random simple record (without without nested records)."
  ((from-list (list (lambda () (record->list* ($blob)))
                    (lambda () (record->list* ($id ($string))))))))

(define ($tagged-list)
  "Return a random record enumerated in the list."
  ((from-list (list (lambda ()
		      (record->list* ($profile)))
		    (lambda ()
		      (record->list* ($scorecard)))
		    (lambda ()
		      (record->list* ($mk-set)))))))

;;;;; Support Generators & Functions
;;;; This section contains library internal definitions that should
;;;; not normally be needed when writing quickcheck tests. These
;;;; functions are used by some of the generators in the rest of the
;;;; library.

;; FIXME: Challenge may well be an abstraction that includes agreed
;; instructions to the client on providing options etc.
(define ($challenge)
  "Return challenge abstraction datum."
  ($string))

(define ($small)
  "Return a integer up to value 10."
  (random-integer 10))

(define (from-list list)
  "Return a random member from LIST."
  (list-ref list (random-integer (length list))))

;; Straight copy from quickcheck.sls
(define (build-list size proc)
  (define (loop i l)
    (if (negative? i)
        l
        (loop (- i 1) (cons (proc i) l))))
  (loop (- size 1) '()))
