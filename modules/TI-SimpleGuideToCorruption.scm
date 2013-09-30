;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (modules ti-plain-guide)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft problem-types open-problems)
  #:use-module (guilecraft problem-types multi-choice-problems)
  #:export (ti-plain-guide-module))

(define ti-plain-guide-module
  (gmodule
   (id 'ti-plain-guide)
   (name "The Anti-Corruption Plain Language Guide")
   (version "0.1")
   (synopsis "Transparency International created this guide in 2009. The Plain Language Guide offers a set of standardised, easy-to-understand definitions, providing readers with concrete examples in practice of how TI approaches these issues. Relevant links are also provided for further background information or research.")
   (description "Long Description")
   (creators "Alex Sassmannshausen")
   (find-out-more "http://www.transparency-international.org")
   (derivation-source "Derivation Source")
   (parts '())))
