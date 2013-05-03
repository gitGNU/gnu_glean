;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (modules ti-plain-guide)
  #:use-module (guilecraft data-types gmodules)
  #:use-module (guilecraft data-types gsets)
  #:use-module (guilecraft problem-types open-problems)
  #:use-module (guilecraft problem-types multi-choice-problems)
  #:use-module (guilecraft gmodule-manager)
  #:export (ti-plain-guide-module))

(define ti-plain-guide-module
  (gmod_make-gmodule
   (id 'ti-plain-guide)
   (name "The Anti-Corruption Plain Language Guide")
   (version "0.1")
   (description "Transparency International created this guide in 2009. The Plain Language Guide offers a set of standardised, easy-to-understand definitions, providing readers with concrete examples in practice of how TI approaches these issues. Relevant links are also provided for further background information or research.")
   (long-description "Long Description")
   (creators "Alex Sassmannshausen")
   (find-out-more "http://www.transparency-international.org")
   (derivation-source "Derivation Source")
   (parts '())))

(gman_add-gmodule ti-plain-guide-module)
