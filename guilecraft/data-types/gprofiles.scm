;;; guilecraft --- Fast learning tool.         -*- coding: utf-8 -*-

(define-module (guilecraft data-types gprofiles)
  #:use-module (guilecraft data-types scorecards)
  #:use-module (guilecraft hash)
  #:use-module (guilecraft utils)
  #:use-module (rnrs)
  #:use-module (srfi srfi-1)
  #:export (make-profile
            make-bare-profile
            profile?
            profile-name
            profile-id
            profile-prof-server
            profile-mod-server
            profile-active-modules
            profile-scorecard

            make-id
            id-symbol
            id-stamp
            id?

            first-active-module
            rest-active-modules
            parse-active-modules
            empty-active-modules?
            active-modules-ids
            active-modules-hashes
            create-profile-id
            update-id
            id->symbol
            name->hash
            id->hash
            profile-hash
            update-profile
            ))

(define profile-rtd
  (make-record-type-descriptor 'profile #f #f #f #f
                               '#((immutable name)
                                  (immutable id)
                                  (immutable prof-server)
                                  (immutable mod-server)
                                  (immutable active-modules)
                                  (immutable scorecard))))
(define profile-rcd
  (make-record-constructor-descriptor
   profile-rtd #f
   (lambda (new)
     (lambda (name id prof-server mod-server active-modules scorecard)
       (cond ((not (string? name))
              (raise '(make-profile invalid-name)))
             ((not (id? id))
              (raise '(make-profile invalid-id)))
             ((not (string? prof-server))
              (raise '(make-profile invalid-prof-server)))
             ((not (string? mod-server))
              (raise '(make-profile invalid-mod-server)))
             ((not (list? active-modules))
              (raise '(make-profile invalid-active-modules)))
             ((not (scorecard? scorecard))
              (raise '(make-profile invalid-scorecard)))
             (else (new name id prof-server mod-server
                        active-modules scorecard)))))))
(define bare-profile-rcd
  (make-record-constructor-descriptor
   profile-rtd #f
   (lambda (new)
     (lambda (name prof-server mod-server)
       (new name (create-profile-id name) prof-server mod-server
            '() (make-empty-scorecard))))))
(define make-profile (record-constructor profile-rcd))
(define make-bare-profile (record-constructor bare-profile-rcd))
(define profile? (record-predicate profile-rtd))
(define profile-name (record-accessor profile-rtd 0))
(define profile-id (record-accessor profile-rtd 1))
(define profile-prof-server (record-accessor profile-rtd 2))
(define profile-mod-server (record-accessor profile-rtd 3))
(define profile-active-modules (record-accessor profile-rtd 4))
(define profile-scorecard (record-accessor profile-rtd 5))

(define id-rtd
  (make-record-type-descriptor 'id #f #f #f #f
                               '#((immutable symbol)
                                  (immutable stamp))))
(define id-rcd
  (make-record-constructor-descriptor id-rtd #f #f))
(define make-id (record-constructor id-rcd))
(define id? (record-predicate id-rtd))
(define id-symbol (record-accessor id-rtd 0))
(define id-stamp (record-accessor id-rtd 1))

;;;; Convenience
(define (parse-active-modules active-modules)
  (fold (lambda (current previous)
          (if (and previous
                   (pair? current)
                   (blobhash? (car current))  ; minhash
                   (blobhash? (cdr current))) ; fullhash
              #t #f))
        #t active-modules))

(define first-active-module
  (lambda (active-modules)
    (car active-modules)))

(define rest-active-modules
  (lambda (active-modules)
    (cdr active-modules)))

(define empty-active-modules?
  (lambda (active-modules)
    (null? active-modules)))

(define (active-modules-hashes active-modules)
  "Return the blobhashes for every entry in ACTIVE-MODULES."
  (map cdr active-modules))
(define (active-modules-ids active-modules)
  "Return the ids for every entry in ACTIVE-MODULES."
  (map car active-modules))

(define (create-profile-id name)
  "Generate the ID on the basis of name and current-time."
  (make-id (string->symbol name) (string->symbol
                                  (number->string
                                   (current-time)))))

(define (update-id profile)
  "Convenience procedure to generate new ID for existing profile."
  (create-profile-id (profile-name profile)))

(define (id->symbol profile-id)
  (if (id? profile-id)
      (symbol-append (id-symbol profile-id)
                     (id-stamp profile-id))
      (assertion-violation
       'id->symbol
       "PROFILE-ID is not a profile id."
       profile-id)))

(define (id->hash profile-id)
  (if (id? profile-id)
      (symbol-hash (id->symbol profile-id))
      (assertion-violation
       'id->hash
       "PROFILE-ID is not a profile id."
       profile-id)))

(define (name->hash profile-name)
  (if (string? profile-name)
      (string-hash profile-name)
      (assertion-violation
       'name->hash
       "PROFILE-NAME is not a profile string."
       profile-name)))

(define (update-profile field value profile)
  (define (update-if this-field accessor)
    (if (eqv? this-field field) value (accessor profile)))
  (make-profile (update-if 'name           profile-name)
                ;; obsolete?
                (create-profile-id (update-if 'name profile-name))
                (update-if 'prof-server    profile-prof-server)
                (update-if 'mod-server     profile-mod-server)
                (update-if 'active-modules profile-active-modules)
                (update-if 'scorecard      profile-scorecard)))

(define* (profile-hash name password)
  (sha256-string name password))
