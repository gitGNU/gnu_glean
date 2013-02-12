#! /usr/bin/guile -s

coding:utf-8
!#

;;; Commentary:

;; Data-types defines the creators and selectors
;; used throughout guilecraft. It is loaded by
;; all other modules.

;;; Code:

(define-module (guilecraft data-types)
  #:use-module (srfi srfi-9)
  #:export (make-gc-module
            make-level
            make-problem
            make-question
            make-solution
            make-tag
            gc-module?
            level?
            problem?
            question?
            solution?
            tag?
            level
            problem
            question
            solution
            tag
            type
            content))

(define (make-question content)
  "Fundamental data type."
  (cons 'question content))

(define (make-solution content)
  "Fundamental data type carrying the value of the right answer for a problem."
  (cons 'solution content))

(define (make-tag content)
  "Fundamental data-type carrying meta-data about a problem, allowing problems to be grouped by problem area.

Example: two problems, (+ 2 2) and (+ 7 10), might both have the tag 'addition, as both are instances of that operation."
  (cons 'tag content))

(define (make-open-problem question solution tag)
  "Open-problem compound data-type creator. 
Open problems are a type of problem. Problems are the fundamental data-type used in interactions with users. At heart they consist of a question, a solution and a tag for grouping different problems together. Problems are subsumed within levels and modules in turn."
  (list 'problem question solution tag))

(define (make-level . problems)
  "Level compound data-type creator. 
Levels are logical groupings of problems for guilecraft. They consist of any number of problems which are combined to set a soft limit to the number of problems that are added to the balancing algorithm at any one time."
  (cons 'level problems))

(define (make-gc-module . levels)
    "Module compound data-type creator. 
  Modules are the primary way by which sets of problems, through the grouping of levels, are linked into a whole in Guilecraft. Modules consist of any number of levels.
Modules should be thought of as narrating devices which provide a context through which users can engage with study materials."
    (cons 'module levels))

(define (type guilecraft-base-data)
  (car guilecraft-base-data))
(define (content guilecraft-base-data)
  (cdr guilecraft-base-data))

(define (question problem)
  "Problem compound data-type caar selector."
  (cadr problem))

(define (solution problem)
  "Problem compound data-type cdar selector."
  (caddr problem))

(define (tag problem)
  "Problem compound data-type selector to read the tag.
Problems are grouped by tag, which allows for the ranking 
of problems as a group."
 (cadddr problem))

(define (problem index level)
  (if (> index (length (content level)))
      'index-out-of-range
      (list-ref level index)))

(define (level index module)
  (if (> index (length (content module)))
      'index-out-of-range
      (list-ref module index)))

(define (gc-module? obj)
  "test whether @var{obj} is a guilecraft module"
  (if (eqv? (type obj) 'module)
      #t
      #f))
(define (level? obj)
  "test whether @var{obj} is a guilecraft level."
  (if (eqv? (type obj) 'level)
      #t
      #f))
(define (problem? obj)
  "test whether @var{obj} is a guilecraft problem."
  (if (eqv? (type obj) 'problem)
      #t
      #f))
(define (question? obj)
  "test whether @var{obj} is a guilecraft question"
  (if (eqv? (type obj) 'question)
      #t
      #f))
(define (solution? obj)
  "test whether @var{obj} is a guilecraft solution"
  (if (eqv? (type obj) 'solution)
      #t
      #f))
(define (tag? obj)
  "test whether @var{obj} is a guilecraft tag"
  (if (eqv? (type obj) 'tag)
      #t
      #f))
