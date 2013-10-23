;;; guilecraft --- fast learning tool.         -*- coding: utf-8 -*-

;;; Commentary:
;;
;; Gmodule-Manager defines an interface to centrally store all known
;; gmodules. The table is indexed on gmodule-id and it returns a
;; gmodule object.
;; Gmodule-Manager also defines an interface 

(define-module (guilecraft problem-type-manager)
  #:use-module (guilecraft data-manager)
  #:use-module (guilecraft utils)
  #:use-module (guilecraft data-types sets)
    #:use-module (rnrs)
  #:export (ptm_add-problem-type
	    ptm_get-problem-type

	    ptm_assess-answer
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

(define ptm_add-problem-type
  (lambda (problem-type-provider prototype-problem-type)
    "Convenience procedure to add a given gmodule to the
gmodule-manager."
    (problem-type-manager 'put problem-type-provider prototype-problem-type)))

(define get-problem-type-provider
  (lambda (problem)
    "Convenience procedure to retrieve a given problem-type-provider
from the problem-type-manager table."
    (problem-type-manager 'get (ptm_get-problem-type problem))))

(define ptm_get-problem-type
  (lambda (problem)
    (record-type-name (record-type-descriptor problem))))

;; define a data-manager table using the result of record-type-name as key
(define problem-type-manager (dman_data-manager ptm_get-problem-type))
