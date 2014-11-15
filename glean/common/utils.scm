;; utils.scm --- general utility functions          -*- coding: utf-8 -*-
;;
;; Copyright (C) 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>,
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
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
;; Some utility functions and quick hacks. Some of this is the result of not
;; having implemented a proper solution to a problem yet (e.g. the logging
;; functions).
;; Some of this is stolen from Ludovic Courtès' Guix.
;; 
;;; Code:

(define-module (glean common utils)
  #:use-module (ice-9 match)
  #:use-module (rnrs records inspection)
  #:use-module (rnrs records syntactic)
  #:use-module (rnrs records procedural)
  #:use-module (srfi srfi-26)
  #:export (
            _
            N_
            seq
            flatten
            mkdir-p
            memoize
            rprinter
            display=>
            emit-usage
            emit-version
            %gettext-domain%
            warning
            report-error
            leave
            part-ways
            program-name
            logger
            log-level
            log-levels
            make-logger
            inform
            caution
            insist
            exclaim
            ))


;;;;; General Functionality

(define %gettext-domain% "glean")
(define _ (cut gettext <> %gettext-domain%))
(define N_ (cut ngettext <> <> <> %gettext-domain%))

(define (display=> x)
  "Emit X to stdout, and return it afterwards.  X can be any Scheme object,
and will simply be printed as format's '~a' would interpret it."
  (format #t "~a\n" x) x)

(define (seq a b)
  "Return a list, counting upwards, from A to B (inclusive)."
  (define (helper curr result)
    (if (> curr b) (reverse result) (helper (1+ curr)
                                            (cons curr result))))
  (if (> a b) '() (helper a '())))

(define (flatten obj)
  "Return '() if OBJ is null?, OBJ wrapped in a list if it is not a pair, or a
flattened proper list containing the elements of OBJ if OBJ is either a list,
pair or improper list."
  (cond ((null? obj) '())
        ((not (pair? obj)) (list obj))
        (else
         (append (flatten (car obj))
                 (flatten (cdr obj))))))

(define (mkdir-p dir)
  "Create directory DIR and all its ancestors as necessary."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

(define (memoize proc)
  "Return a memoizing version of PROC."
  (let ((cache (make-hash-table)))
    (lambda args
      (let ((results (hash-ref cache args)))
        (if results
            (apply values results)
            (let ((results (call-with-values (lambda ()
                                               (apply proc args))
                             list)))
              (hash-set! cache args results)
              (apply values results)))))))


;;;; UI Convenience
;;;
;;; The procedures defined in this section are primarily taken from Guix.
;;; Their intended role is to provide unified message formats for use in UI
;;; situations.
;;;
;;; There is a strict difference between logging and UI messages: the latter
;;; should always be i18nised, and are aimed at the end-user; the former don't
;;; need to be i18nised, and are aimed at curious users and hackers.

(define glean-warning-port
  (make-parameter (current-warning-port)))

(define program-name
  ;; Name of the command-line program currently executing, or #f.
  (make-parameter #f))

(define-syntax-rule (define-diagnostic name prefix)
  "Create a diagnostic macro (i.e., NAME), which will prepend PREFIX to all
messages."
  (define-syntax name
    (lambda (x)
      (define (augmented-format-string fmt)
        (string-append "~:[~*~;glean ~a: ~]~a" (syntax->datum fmt)))

      (syntax-case x ()
        ((name (underscore fmt) args (... ...))
         (and (string? (syntax->datum #'fmt))
              (free-identifier=? #'underscore #'_))
         (with-syntax ((fmt*   (augmented-format-string #'fmt))
                       (prefix (datum->syntax x prefix)))
           #'(format (glean-warning-port) (gettext fmt*)
                     (program-name) (program-name) prefix
                     args (... ...))))
        ((name (N-underscore singular plural n) args (... ...))
         (and (string? (syntax->datum #'singular))
              (string? (syntax->datum #'plural))
              (free-identifier=? #'N-underscore #'N_))
         (with-syntax ((s      (augmented-format-string #'singular))
                       (p      (augmented-format-string #'plural))
                       (prefix (datum->syntax x prefix)))
           #'(format (glean-warning-port)
                     (ngettext s p n %gettext-domain)
                     (program-name) (program-name) prefix
                     args (... ...))))))))

(define-diagnostic warning "warning: ") ; emit a warning

(define-diagnostic advice "info: ")

(define-diagnostic report-error "error: ")

(define-syntax-rule (leave args ...)
  "Emit an error message and exit."
  (begin
    (report-error args ...)
    (exit 1)))

(define-syntax-rule (part-ways args ...)
  "Emit advice message and exit with success."
  (begin
    (advice args ...)
    (exit 0)))


;;;;; Logging Functions
;;;
;;; Logging in Glean is handled by 2 parameters: log-level and logger.
;;;
;;; - log-level identifies desired verbosity, but its levels also specify the
;;;   urgency of individual log messages. For instance, a user may indicate
;;;   that they are interested in log messages of level 'insist or higher: as
;;;   a result, any messages passed to logger with a priority of 'insist or
;;;   higher will be emitted for the user (if logging is switched on).
;;; - logger returns a procedure that will handle the actual logging. That is,
;;;   logging messages will always take the form of:
;;;   ((logger) 'priority '(message))
;;;   where '(message is a list of format-string and arguments.
;;;   logger is, normally generated using the 'make-logger' procedure.
;;;
;;; Monadic logging operates at a higher level: it uses an intermediate
;;; construct called mlogger, defined in (glean common monads).  Look there
;;; for more information.

(define log-levels (const '(exclaim caution insist inform all)))

(define (make-logger verbose log default-log-file)
  "Return a logging procedure that, if VERBOSE is #t logs to stdout, if LOG is
#t logs to a log file identified by DEFAULT-LOG-FILE.  If neither are #t, do
not log."
  (define (logger port)
    (lambda (priority msg)
      (when (relevant? priority (log-level))
        (if (string? port)
            (catch 'system-error
              (lambda ()
                (let ((port (open-file port "a")))
                  (apply format port msg)
                  (close-port port)))
              (lambda (key . args)
                (leave (_ "logging failure: ~a.~%")
                       (string-join (caddr args) " "))))
            (apply format port msg)))))
  (cond (verbose                       (logger (current-output-port)))
        ((and log (string? log))       (logger log))
        ((and log default-log-file)    (logger default-log-file))
        (else                          (const #f))))

(define logger
  ;; Identify default behaviour for logging purposes
  ;; Defaults to a constant that simply returns #f.
  (make-parameter (make-logger #f #f #f)))

(define log-level
  ;; Identify the default log-level.
  (make-parameter 'debug))

(define (inform . msg)
  "Log a message that should only be useful in debug/dev context."
  ((logger) 'inform msg))

(define (insist . msg)
  "Log a message that might be useful during normal operation."
  ((logger) 'insist msg))

(define (caution . msg)
  "Log a message indicating an unexpected situation which is not critical."
  ((logger) 'caution msg))

(define (exclaim . msg)
  "Log a high-priority message, usually just before throwing an error."
  ((logger) 'exclaim msg))

(define (rprinter record)
  "Returns #f if RECORD is not a record, #t otherwise. Recursively
prints RECORD and its fields as a side-effect if #t. This is a debugging
tool."

  (if (record? record)
      (let* ((rtd (record-rtd record))
             (fields (vector->list (record-type-field-names rtd)))
             (length (length fields))
             (port (current-output-port)))

        (define (print-fields remaining index)
          (cond ((null? remaining)
                 #t)
                (else
                 (let ((field-value ((record-accessor rtd index)
                                     record)))
                   (cond ((record? field-value)
                          (format port "   ~a: ~a\n" (car remaining)
                                  field-value)
                          (rprinter field-value))
                         ((and (list? field-value)
                               (not (null? field-value))
                               (record? (car field-value)))
                          (for-each (lambda (arg)
                                      (rprinter arg))
                                    field-value))
                         (else
                          (format port "   ~a: ~a\n" (car remaining)
                                  field-value))))
                 (print-fields (cdr remaining) (1+ index)))))

        (format port "record: ~a\n" (record-type-name rtd))
        (print-fields fields 0)
        (format port ":end:\n"))
      #f))

(define (relevant? urgency level)
  "Return #t if PRIORITY is higher than %LOG-LEVEL%. PRIORITY should be one of
the symbols (in increasing order of importance) 'all, 'inform, 'insist,
'caution or 'exclaim."
  (define (weigh rating)
    ;; Should use log-levels to create this list, but that seems ugly for now…
    (match rating
      ('exclaim   0)
      ('caution   1)
      ('insist    2)
      ('inform    3)
      (_          4)))
  (<= (weigh urgency) (weigh level)))


;;;;; Package Procedures
;;; Some procedures to be used by the various launchers of Guile to provide a
;;; unified CLI.

(define (emit-version package-name version)
  "Output a version message and exit."
  (format #t
          "~a ~a
Copyright (C) 2014 Alex Sassmannshausen.
~a comes with ABSOLUTELY NO WARRANTY.
License: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is Free Software: you are free to change and redistribute it under the
terms of the above license.

For more information about these matters, see the file named COPYING.
"
          package-name version package-name))

(define* (emit-usage command synopsis description options messages
                     #:key (subcommand ""))
  "Return a usage message formatted in the following fashion:
Usage: COMMAND [--LONG-OPT | -SHORT-OPT]
               ...
  LONG-OPT:   MESSAGE
"
  (define (name-char-pair option)
    (match option
      ((name ('single-char char) ('value id))
       `(,(symbol->string name) ,(make-string 1 char) . ,id))
      ((name . _)
       `(,(symbol->string name) . #f))
      (_ (error "USAGE -- Unexpected option:" option))))
  (define (info-line pair)
    (match pair
      ((name . #f)
       (string-append "[--" name "]"))
      ((name char . #t)
       (string-append "[--" name "=VALUE | -" char " VALUE]"))
      ((name char . 'optional)
       (string-append "[--" name "[=VALUE] | -" char " [VALUE]]"))
      ((name char . #f)
       (string-append "[--" name " | -" char "]"))
      (_ (error "USAGE -- Should be impossible:" pair))))
  (define (spaces length)
    (if (negative? length)
        (error "USAGE -- option name is too long!")
        (make-string length #\ )))
  
  (let ((pairs (map name-char-pair options)))
    (match (map info-line pairs)
      ((first . rest)
       (if subcommand
           (format #t "Usage:     ~a ~a ~a\n" command subcommand first)
           (format #t "Usage:     ~a ~a\n" command first))
       (for-each (lambda (info)
                   (format #t "           ~a ~a\n"
                           (spaces (+ (string-length command)
                                      (if subcommand
                                          (1+ (string-length subcommand))
                                          0)))
                           info))
                 rest)))
    (format #t "\n~a\n" synopsis)
    (for-each (lambda (pair message)
                (match pair
                  ((name . char)
                   (format #t "      ~a:~a~a\n"
                           name
                           (spaces (- 15 (string-length name)))
                           message))))
              pairs messages)
    (format #t "\n~a\n" description)))

;;; utils.scm ends here
