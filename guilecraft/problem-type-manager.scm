;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;; Commentary:
;;
;; Gmodule-Manager defines an interface to centrally store all known
;; gmodules. The table is indexed on gmodule-id and it returns a
;; gmodule object.
;; Gmodule-Manager also defines an interface 

(define-module (guilecraft problem-type-manager)
  #:use-module (guilecraft utils)
  #:use-module (guilecraft data-types sets)
    #:use-module (rnrs)
  #:export (ptm_assess-answer
	    ptm_get-challenge))


;; High Level Evaluate Answer
;;; 

(define ptm_assess-answer
  (lambda (player-answer problem)
    "Currently returns #t if @var{player-answer} is assessed
successfully against @var{whirligig}'s current problem's solution.
This is a high-level function, called directly from the UI."
    ;; Return the problem type, which is also the exposed procedure
    ;; accepting the 'assess message
    (if (and player-answer
	     (problem? problem))
	((p-function (problem-p problem))
	 player-answer
	 (s-text (problem-s problem)))
	(assertion-violation
	 'assess-answer
	 "PLAYER-ANSWER is #f or PROBLEM is not a problem!"
	 player-answer problem))))

(define (ptm_get-challenge problem)
  "Return a challenge, depending on the type of problem."
  ;; Return and apply the problem-type-provider if it exists, which
  ;; is also the exposed procedure accepting the 'get-challenge
  ;; message 
  (q-text (problem-q problem)))
  ;; (if (get-problem-type-provider problem)
  ;;     ((get-problem-type-provider problem) 'get-challenge problem)
  ;;     'ptm_get-challenge-unknown-problem-type))
