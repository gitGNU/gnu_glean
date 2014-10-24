;; greek-alphabet.scm --- discipline for learning α-ω    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 24 October 2014
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
;; Define a discipline to learn the ancient Greek alphabet and its symbols.
;;
;; α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ σ τ υ φ χ ψ ω
;; Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ Σ Τ Υ Φ Χ Ψ Ω
;;
;;; Code:

(define-module (glean store greek-alphabet)
  #:use-module (glean library core-templates)
  #:export     (greek-alphabet))


;;;; Local Templates

(define (letter-set id name synopsis . problems)
  (set id
       #:name name
       #:synopsis synopsis
       #:contents problems))


;;;; Exercises
;;;
;;; For clarity we define the exercises at the top level.  As a result we
;;; define the discipline itself at the very bottom of the file.

(define alpha
  (letter-set 'alpha "Problems for Alpha"
              "Problems relating to the letter Alpha."
              (problem (q "Which of the following is ‘α‘?")
                       (s "alpha")
                       (o "lambda")
                       (o "alpha")
                       (o "eta"))
              (problem (q "What letter is ‘α‘?")
                       (s "alpha"))
              (problem (q "Which of the following is ‘Α‘?")
                       (s "alpha")
                       (o "zeta")
                       (o "nu")
                       (o "alpha"))
              (problem (q "What letter is ‘Α‘?")
                       (s "alpha"))))

(define beta
  (letter-set 'beta "Problems for Beta"
              "Problems relating to the letter Beta."
              (problem (q "Which of the following is ‘β‘?")
                       (s "beta")
                       (o "beta")
                       (o "omicron")
                       (o "epsilon"))
              (problem (q "What letter is ‘β‘?")
                       (s "beta"))
              (problem (q "Which of the following is ‘Β‘?")
                       (s "beta")
                       (o "iota")
                       (o "beta")
                       (o "chi"))
              (problem (q "What letter is ‘Β‘?")
                       (s "beta"))))

(define gamma
  (letter-set 'gamma "Problems for Gamma"
              "Problems relating to the letter Gamma."
              (problem (q "Which of the following is ‘γ‘?")
                       (s "gamma")
                       (o "sigma")
                       (o "psi")
                       (o "gamma"))
              (problem (q "What letter is ‘γ‘?")
                       (s "gamma"))
              (problem (q "Which of the following is ‘Γ‘?")
                       (s "gamma")
                       (o "gamma")
                       (o "sigma")
                       (o "xi"))
              (problem (q "What letter is ‘Γ‘?")
                       (s "gamma"))))

(define delta
  (letter-set 'delta "Problems for Delta"
              "Problems relating to the letter Delta."
              (problem (q "Which of the following is ‘δ‘?")
                       (s "delta")
                       (o "delta")
                       (o "rho")
                       (o "kappa"))
              (problem (q "What letter is ‘δ‘?")
                       (s "delta"))
              (problem (q "Which of the following is ‘Δ‘?")
                       (s "delta")
                       (o "lambda")
                       (o "delta")
                       (o "mu"))
              (problem (q "What letter is ‘Δ‘?")
                       (s "delta"))))

(define epsilon
  (letter-set 'epsilon "Problems for Epsilon"
              "Problems relating to the letter Epsilon."
              (problem (q "Which of the following is ‘ε‘?")
                       (s "epsilon")
                       (o "tau")
                       (o "beta")
                       (o "epsilon"))
              (problem (q "What letter is ‘ε‘?")
                       (s "epsilon"))
              (problem (q "Which of the following is ‘Ε‘?")
                       (s "epsilon")
                       (o "epsilon")
                       (o "phi")
                       (o "omega"))
              (problem (q "What letter is ‘Ε‘?")
                       (s "epsilon"))))

(define zeta
  (letter-set 'zeta "Problems for Zeta"
              "Problems relating to the letter Zeta."
              (problem (q "Which of the following is ‘ζ‘?")
                       (s "zeta")
                       (o "zeta")
                       (o "sigma")
                       (o "theta"))
              (problem (q "What letter is ‘ζ‘?")
                       (s "zeta"))
              (problem (q "Which of the following is ‘Ζ‘?")
                       (s "zeta")
                       (o "zeta")
                       (o "mu")
                       (o "nu"))
              (problem (q "What letter is ‘Ζ‘?")
                       (s "zeta"))))

(define eta
  (letter-set 'eta "Problems for Eta"
              "Problems relating to the letter Eta."
              (problem (q "Which of the following is ‘η‘?")
                       (s "eta")
                       (o "iota")
                       (o "rho")
                       (o "eta"))
              (problem (q "What letter is ‘η‘?")
                       (s "eta"))
              (problem (q "Which of the following is ‘Η‘?")
                       (s "eta")
                       (o "kappa")
                       (o "eta")
                       (o "omicron"))
              (problem (q "What letter is ‘Η‘?")
                       (s "eta"))))

(define theta
  (letter-set 'theta "Problems for Theta"
              "Problems relating to the letter Theta."
              (problem (q "Which of the following is ‘θ‘?")
                       (s "theta")
                       (o "alpha")
                       (o "theta")
                       (o "beta"))
              (problem (q "What letter is ‘θ‘?")
                       (s "theta"))
              (problem (q "Which of the following is ‘Θ‘?")
                       (s "theta")
                       (o "gamma")
                       (o "delta")
                       (o "theta"))
              (problem (q "What letter is ‘Θ‘?")
                       (s "theta"))))

(define iota
  (letter-set 'iota "Problems for Iota"
              "Problems relating to the letter Iota."
              (problem (q "Which of the following is ‘ι‘?")
                       (s "iota")
                       (o "iota")
                       (o "lambda")
                       (o "xi"))
              (problem (q "What letter is ‘ι‘?")
                       (s "iota"))
              (problem (q "Which of the following is ‘Ι‘?")
                       (s "iota")
                       (o "tau")
                       (o "omicron")
                       (o "iota"))
              (problem (q "What letter is ‘Ι‘?")
                       (s "iota"))))

(define kappa
  (letter-set 'kappa "Problems for Kappa"
              "Problems relating to the letter Kappa."
              (problem (q "Which of the following is ‘κ‘?")
                       (s "kappa")
                       (o "lambda")
                       (o "omicron")
                       (o "kappa"))
              (problem (q "What letter is ‘κ‘?")
                       (s "kappa"))
              (problem (q "Which of the following is ‘Κ‘?")
                       (s "kappa")
                       (o "lambda")
                       (o "kappa")
                       (o "eta"))
              (problem (q "What letter is ‘Κ‘?")
                       (s "kappa"))))

(define lambda
  (letter-set 'lambda "Problems for Lambda"
              "Problems relating to the letter Lambda."
              (problem (q "Which of the following is ‘λ‘?")
                       (s "lambda")
                       (o "lambda")
                       (o "mu")
                       (o "eta"))
              (problem (q "What letter is ‘λ‘?")
                       (s "lambda"))
              (problem (q "Which of the following is ‘Λ‘?")
                       (s "lambda")
                       (o "pi")
                       (o "lambda")
                       (o "nu"))
              (problem (q "What letter is ‘Λ‘?")
                       (s "lambda"))))

(define mu
  (letter-set 'mu "Problems for Mu"
              "Problems relating to the letter Mu."
              (problem (q "Which of the following is ‘μ‘?")
                       (s "mu")
                       (o "epsilon")
                       (o "psi")
                       (o "mu"))
              (problem (q "What letter is ‘μ‘?")
                       (s "mu"))
              (problem (q "Which of the following is ‘Μ‘?")
                       (s "mu")
                       (o "iota")
                       (o "mu")
                       (o "upsilon"))
              (problem (q "What letter is ‘Μ‘?")
                       (s "mu"))))

(define nu
  (letter-set 'nu "Problems for Nu"
              "Problems relating to the letter Nu."
              (problem (q "Which of the following is ‘ν‘?")
                       (s "nu")
                       (o "zeta")
                       (o "nu")
                       (o "rho"))
              (problem (q "What letter is ‘ν‘?")
                       (s "nu"))
              (problem (q "Which of the following is ‘Ν‘?")
                       (s "nu")
                       (o "upsilon")
                       (o "nu")
                       (o "xi"))
              (problem (q "What letter is ‘Ν‘?")
                       (s "nu"))))

(define xi
  (letter-set 'xi "Problems for Xi"
              "Problems relating to the letter Xi."
              (problem (q "Which of the following is ‘ξ‘?")
                       (s "xi")
                       (o "kappa")
                       (o "pi")
                       (o "xi"))
              (problem (q "What letter is ‘ξ‘?")
                       (s "xi"))
              (problem (q "Which of the following is ‘Ξ‘?")
                       (s "xi")
                       (o "xi")
                       (o "omicron")
                       (o "upsilon"))
              (problem (q "What letter is ‘Ξ‘?")
                       (s "xi"))))

(define omicron
  (letter-set 'omicron "Problems for Omicron"
              "Problems relating to the letter Omicron."
              (problem (q "Which of the following is ‘ο‘?")
                       (s "omicron")
                       (o "rho")
                       (o "omicron")
                       (o "eta"))
              (problem (q "What letter is ‘ο‘?")
                       (s "omicron"))
              (problem (q "Which of the following is ‘Ο‘?")
                       (s "omicron")
                       (o "lambda")
                       (o "zeta")
                       (o "omicron"))
              (problem (q "What letter is ‘Ο‘?")
                       (s "omicron"))))

(define pi
  (letter-set 'pi "Problems for Pi"
              "Problems relating to the letter Pi."
              (problem (q "Which of the following is ‘π‘?")
                       (s "pi")
                       (o "pi")
                       (o "sigma")
                       (o "delta"))
              (problem (q "What letter is ‘π‘?")
                       (s "pi"))
              (problem (q "Which of the following is ‘Π‘?")
                       (s "pi")
                       (o "upsilon")
                       (o "pi")
                       (o "xi"))
              (problem (q "What letter is ‘Π‘?")
                       (s "pi"))))

(define rho
  (letter-set 'rho "Problems for Rho"
              "Problems relating to the letter Rho."
              (problem (q "Which of the following is ‘ρ‘?")
                       (s "rho")
                       (o "rho")
                       (o "chi")
                       (o "psi"))
              (problem (q "What letter is ‘ρ‘?")
                       (s "rho"))
              (problem (q "Which of the following is ‘Ρ‘?")
                       (s "rho")
                       (o "omega")
                       (o "rho")
                       (o "psi"))
              (problem (q "What letter is ‘Ρ‘?")
                       (s "rho"))))

(define sigma
  (letter-set 'sigma "Problems for Sigma"
              "Problems relating to the letter Sigma."
              (problem (q "Which of the following is ‘σ‘?")
                       (s "sigma")
                       (o "omega")
                       (o "sigma")
                       (o "theta"))
              (problem (q "What letter is ‘σ‘?")
                       (s "sigma"))
              (problem (q "Which of the following is ‘Σ‘?")
                       (s "sigma")
                       (o "sigma")
                       (o "epsilon")
                       (o "eta"))
              (problem (q "What letter is ‘Σ‘?")
                       (s "sigma"))))

(define tau
  (letter-set 'tau "Problems for Tau"
              "Problems relating to the letter Tau."
              (problem (q "Which of the following is ‘τ‘?")
                       (s "tau")
                       (o "upsilon")
                       (o "kappa")
                       (o "tau"))
              (problem (q "What letter is ‘τ‘?")
                       (s "tau"))
              (problem (q "Which of the following is ‘Τ‘?")
                       (s "tau")
                       (o "zeta")
                       (o "tau")
                       (o "xi"))
              (problem (q "What letter is ‘Τ‘?")
                       (s "tau"))))

(define upsilon
  (letter-set 'upsilon "Problems for Upsilon"
              "Problems relating to the letter Upsilon."
              (problem (q "Which of the following is ‘υ‘?")
                       (s "upsilon")
                       (o "alpha")
                       (o "nu")
                       (o "upsilon"))
              (problem (q "What letter is ‘υ‘?")
                       (s "upsilon"))
              (problem (q "Which of the following is ‘Υ‘?")
                       (s "upsilon")
                       (o "gamma")
                       (o "upsilon")
                       (o "omicron"))
              (problem (q "What letter is ‘Υ‘?")
                       (s "upsilon"))))

(define phi
  (letter-set 'phi "Problems for Phi"
              "Problems relating to the letter Phi."
              (problem (q "Which of the following is ‘φ‘?")
                       (s "phi")
                       (o "lambda")
                       (o "phi")
                       (o "alpha"))
              (problem (q "What letter is ‘φ‘?")
                       (s "phi"))
              (problem (q "Which of the following is ‘Φ‘?")
                       (s "phi")
                       (o "phi")
                       (o "omicron")
                       (o "zeta"))
              (problem (q "What letter is ‘Φ‘?")
                       (s "phi"))))

(define chi
  (letter-set 'chi "Problems for Chi"
              "Problems relating to the letter Chi."
              (problem (q "Which of the following is ‘χ‘?")
                       (s "chi")
                       (o "psi")
                       (o "xi")
                       (o "chi"))
              (problem (q "What letter is ‘χ‘?")
                       (s "chi"))
              (problem (q "Which of the following is ‘Χ‘?")
                       (s "chi")
                       (o "chi")
                       (o "eta")
                       (o "tau"))
              (problem (q "What letter is ‘Χ‘?")
                       (s "chi"))))

(define psi
  (letter-set 'psi "Problems for Psi"
              "Problems relating to the letter Psi."
              (problem (q "Which of the following is ‘ψ‘?")
                       (s "psi")
                       (o "psi")
                       (o "beta")
                       (o "upsilon"))
              (problem (q "What letter is ‘ψ‘?")
                       (s "psi"))
              (problem (q "Which of the following is ‘Ψ‘?")
                       (s "psi")
                       (o "phi")
                       (o "nu")
                       (o "psi"))
              (problem (q "What letter is ‘Ψ‘?")
                       (s "psi"))))

(define omega
  (letter-set 'omega "Problems for Omega"
              "Problems relating to the letter Omega."
              (problem (q "Which of the following is ‘ω‘?")
                       (s "omega")
                       (o "xi")
                       (o "omega")
                       (o "rho"))
              (problem (q "What letter is ‘ω‘?")
                       (s "omega"))
              (problem (q "Which of the following is ‘Ω‘?")
                       (s "omega")
                       (o "omega")
                       (o "iota")
                       (o "kappa"))
              (problem (q "What letter is ‘Ω‘?")
                       (s "omega"))))


;;;; Discipline
;;;
;;; We have defined the exercises above, so we now we can simply define the
;;; discipline's meta data

(define greek-alphabet
  (module
    'greek-alphabet
    #:name "The Ancient Greek Alphabet"
    #:version "0.1"
    #:keywords '("education" "languages")
    #:synopsis "Learn the letters of the Ancient Greek alphabet."
    #:description ""
    #:creator "Alex Sassmannshausen"
    #:attribution
    (list (media #:urls '("https://en.wikipedia.org/wiki/Greek_alphabet")))
    #:contents
    (list alpha beta gamma delta epsilon zeta eta theta
          iota kappa lambda mu nu xi omicron pi rho sigma tau upsilon phi chi
          psi omega)
    #:resources
    (list (media #:urls '("https://en.wikipedia.org/wiki/Greek_alphabet")))))

;;; greek-alphabet.scm ends here
