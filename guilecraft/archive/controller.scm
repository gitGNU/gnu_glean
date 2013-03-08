#! /usr/bin/guile -s

coding:utf-8
!#
;;; Commentary:

;;; Controller acts as the interface between client-UIs and
;;; the guilecraft modules/servers.
;;; Benefit is that client UIs merely have to implement some,
;;; or optionally, all, exported controller procedures, which
;;; ensures backward compatibility of the UIs. On the other
;;; hand, extensions to guilecraft can be made in the backend
;;; without having to worry about implementing all necessary 
;;; changes in the UIs.
;;; Centralizing traffic through controller also allows for 
;;; easier development of multiple simultaniously active clients
;;; on one server, and reduces the amount of paths needed between
;;; the lower level modules/servers.
;;; It will risk creating a bottle-neck at controller, which might
;;; have to be resolved by turning controller into a load-balancing
;;; server, and letting session management occur further down, in
;;; an as yet non-existing layer.
  
;;; Code:

(define-module (guilecraft controller)
  #:export (controller)                 ; by client
  #:use-module (guilecraft generator)   ; for compile-question
  #:use-module (guilecraft evaluator)   ; for request-evaluation
)

(define (controller command . parameters)
    (let ((profile "atheia")
          (module "guile")
          (level 1))
      (cond ((eqv? command 'generate-question)
             (compile-question))        ;from Generator
            ((eqv? command 'evaluate-answer)
             (request-evaluation (car parameters))))))
