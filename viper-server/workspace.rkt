#lang racket/base

(require json
         racket/match
         racket/contract/base
         "error-codes.rkt"
         "json-util.rkt"
         "responses.rkt"
         )

;; Author Gavin Gray

;; --------------------
;; Match expanders

(define-json-expander VerifyRequest
  [uri string?]
  [manuallyTriggered boolean?]
  [workspace string?])

(define (execute-command id params)
  (match params
    [(hash-table ['command "Verify"]
                 ['arguments (VerifyRequest #:uri uri
                                            #:manuallyTriggered mt
                                            #:workspace wkspc)])
     (error-response id METHOD-NOT-FOUND "You got here (:)")]
    ;; TODO additional commands
    ;;
    [_
     (error-response id METHOD-NOT-FOUND "You /sortof/ got here (:)")]))

;; --------------------

(provide
 (contract-out
  [execute-command (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]))
