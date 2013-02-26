;;; guilecraft --- Nix package management from Guile.         -*- coding: utf-8 -*-

(define-module (guilecraft gmodules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (
	    ;; gmodule record functions
	    make-gmodule
            gmodule?
            get-name
            get-version
            get-description
            get-long-description
	    get-creator
	    get-find-out-more
	    get-derivation-source
	    get-parts
	    get-gmodule-tags
            get-gmodule-full-name

	    ;; gset record functions
	    gset?
	    make-gset
	    get-tag
	    get-problems
	    get-tag-problems
	    get-tag-challenges
	    get-tag-solutions

	    ;; open-problem record functions
	    open-problem?
	    make-open-problem
	    get-challenge
	    get-solution

	    ;; temp helper functions
	    assess-open-problem?
	    eq-open-problem?
	    make-whirligig 
	    get-next-problem 
	    get-current-problem
	    whirligig-hangar
))

;;; Commentary:
;;;
;;; This module provides a high-level mechanism to define gmodules to 
;;; be used in guilecraft.
;;;
;;; Code:


;; A gmodule.
;; Gmodules consist of meta-data and one or more sets/gmodules
(define-record-type <gmodule>
  (make-gmodule name version description long-description creators find-out-more derivation-source parts)
  gmodule?
  ;; What's the gmodule called?
  (name get-name)                   ; string
  ;; Version should really be a number
  (version get-version)             ; number
  ;; First introduction for users
  (description get-description)     ; one-line descriptive string
  ;; Tutorial & background
  (long-description get-long-description) ; Space for teaching material text
  ;; Credit for work done
  (creators get-creators)           ; <credit> instance
  ;; Further Reading
  (find-out-more get-find-out-more) ; <materials> instance
  ;; Give a shout out to your inspiration
  (derivation-source get-derivation-source) ; <materials> instance
  ;; 'Contents' of your gmodule: 'external' gmodules and 'internal' sets
  (parts get-parts)                 ; list of variables
)

;; Accreditation: Give creds where they are due
(define-record-type <credit>
  (make-credit current-creator past-creator contributor)
  credit?
  ;; The current maintainer of the module
  (current-creator get-current-creator)           ; string
  ;; Honor past work
  (past-creator get-past-creator)              ; string
  ;; regular contributor
  (contributor get-contributor))              ; string

;; Background Information: materials for self-directed
;; study or reference to origin of module.
(define-record-type <materials>
  (make-materials identity reading video audio venue)
  materials?
  ;; Organization, group or person associated with resource
  (identity get-identity)
  ;; Reading: websites, books, pamphlet, etc.
  (reading get-reading)
  (video get-video)
  (audio get-audio)
  ;; Regular haunts: meetups, #channels, etc.
  (venue get-venue))

;;; 
;; Define gsets
;;;

;;;
;; Gsets are a means of grouping problems together under a
;; tag. Guilecraft uses the tag to measure progress against a
;; discipline, and will select a "random" question from the
;; algorithmically determined highest priority gset. The overall
;; topography that should be starting to manifest is one where
;; disciplines are provided by gmodules, which either point to other
;; gmodules (to make use of content that has already been created), or
;; which contain a number of gsets that then allow guilecraft to
;; select problems.

;; Gset: a group of problems to be used as stream source of challenges
(define-record-type <gset>
  (make-gset tag problems)
  gset?
  ;; Tag: the problem subject ID against which Guilecraft
  ;; will keep 'score'
  (tag get-tag)             ; symbol
  ;; Problems: this is a list of problems, generated through 
  ;; the problems interfaces.
  (problems get-problems)             ; list of <problems>
  )


(define get-tag-problems
  (lambda (gset-tag gmodule)
    "Return the challenge/solution pairs subsumed under TAG in a given GMODULE.

get-tag-problems searches gmodule parts and returns '() or the problems subsumed within a tag within a module."
    (define helper
      (lambda (gset-tag gmodule-tag-list)
	(cond ((null? gmodule-tag-list)
	       '())
	      ((eq? gset-tag (get-tag (car gmodule-tag-list)))
	       (get-problems (car gmodule-tag-list)))
	      (else (helper gset-tag (cdr gmodule-tag-list))))))
    (helper gset-tag (get-parts gmodule))))

;;;
;; Define Problems
;;;

;;;
;; Problems are the lowest level data-type used in gmodule definition.
;; They provide a challenge and some form of correct solution.
;; We aim to provide a great variety of problem-types such as:
;; - open-problems: challenges expecting a freely typed answer that 
;; matches the provided solution
;; - multiple-choice: challenges that offer a number of solutions from
;; which the user chooses.
;; - dynamic: challenges are generated dynamically according to rules,
;; the user must type an answer that matches the algorithmically
;; generated solution.
;;
;; Problems should also be able to provide their challenges using
;; different media types: text, audio, video.

;; Open-problem: an individual problem record of the open problem
;; variety.
(define-record-type <open-problem>
  (make-open-problem challenge solution)
  open-problem?
  ; Challenges are posed to players to provide a solution to.
  (challenge get-challenge)      ; String
  ; Solutions are what the player's response is assessed against.
  (solution get-solution)        ; String 
  ) 

;;;
;; Gset Helper Functions
;;;

(define get-tag-challenges
  (lambda (gset-tag gmodule)
    "Return the challenges subsumed within a tag in a given guilecraft module."
    (map get-challenge (get-tag-problems gset-tag gmodule))))

(define get-tag-solutions
  (lambda (gset-tag gmodule)
    "Return the solutions subsumed within a tag in a given guilecraft module."
    (map get-solution (get-tag-problems gset-tag gmodule))))


;;;
;; Gmodule Helper Functions
;;;

(define get-gmodule-full-name 
  (lambda (gmodule)
  "Return the full name of GMODULE--i.e., `NAME — version VERSION'."
    (string-append (get-name gmodule) " — version " (get-version gmodule))))

(define get-gmodule-tags 
  (lambda (gmodule)
    "Return the tags in use in a given guilecraft module."
    (map get-tag (get-parts gmodule))))

;; Helper Functions:
(define assess-open-problem?
  (lambda (player-answer open-problem-solution)
    "Evaluate @var{player-answer} against @var{open-problem-solution}
and return #t if correct or #f if incorrect."
    (eq-open-problem? player-answer open-problem-solution)))

(define eq-open-problem? 
  (lambda (player-answer open-problem-solution)
    "Predicate equivalence for @var{player-answer} and
@var{open-problem-solution}. This is separated into a separate module
to maximise the resilience of the superstructure against low-level changes."
    (if (equal? player-answer open-problem-solution)
	#t
	#f)))

(define get-current-problem
  (lambda (whirligig)
    "Returns the current problem in a given whirligig"
    (whirligig 'current)))
(define get-next-problem
  (lambda (whirligig)
    "Returns the next problem in a given whirligig"
    (whirligig 'next)))

(define issue-challenge
  (lambda (whirligig)
    "Currently returns the next challenge in a given whirligig."
    (get-challenge (get-next-problem whirligig))))

(define assess-answer
  (lambda (player-answer whirligig)
    "Currently returns #t if @var{player-answer} is assessed successfully against @var{whirligig}'s current problem's solution.
This is a high-level function, called directly from the UI."
    (cond ((open-problem? (get-current-problem whirligig))
	   (assess-open-problem? player-answer (get-solution (get-current-problem whirligig))))
	  (else 'not-working))))

(define make-whirligig
  (lambda (gset-tag gmodule-variable)
    "Returns a whirligig, an engine capable of cycling through the problems grouped under a tag, independently of other whirligig instances, when called."
    (let ([counter 0]
	  [max-counter (length (get-tag-problems gset-tag gmodule-variable))]
	  [list (get-tag-problems gset-tag gmodule-variable)]
	  [tag gset-tag])
      (lambda (message)
	(cond ((eq? message 'current)
	       (if (= counter 0)
		   (list-ref list (- max-counter 1))
		   (list-ref list (- counter 1))))
	      ((eq? message 'next)
	       (begin
		 (if (= counter max-counter)
		     (set! counter 0))
		 (set! counter (+ counter 1))
		 (list-ref list (- counter 1))))
	      ((eq? message 'tag)
	       tag)
	      (else #f))))))

(define whirligig-hangar
  (let ([whirligig-list '()])
    (lambda (message gset-tag gmodule-variable)
      "Creates new whirligigs as needed or uses them to retrieve the next question/answer for a gset tag in a module."
      (define helper
	(lambda (tmp-whirligig-list message gset-tag gmodule-variable)
	  (cond ((null? tmp-whirligig-list)
		 (begin
		   (set! whirligig-list (cons (make-whirligig gset-tag gmodule-variable) whirligig-list))
		   ((car whirligig-list) message)))
		((equal? ((car tmp-whirligig-list) 'tag) gset-tag)
		 ((car tmp-whirligig-list) message))
		(else (helper (cdr tmp-whirligig-list) message gset-tag gmodule-variable)))))

      (helper whirligig-list message gset-tag gmodule-variable))))
