#lang racket/base

;; Code taken from racket-langserver:
;; https://github.com/jeapostrophe/racket-langserver/blob/master/debug.rkt

(require racket/file
         racket/runtime-path)

(define debug? #t)

(define-runtime-path df "debug.out.rkt")
(define (maybe-debug-file t)
  (when debug?
    (display-to-file t df #:exists 'replace)))

(define-runtime-path dp "debug.log")
(define (maybe-debug-log m)
  (when debug?
    (with-output-to-file dp
      #:exists 'append
      (lambda ()
        (writeln m)))))

(provide
 maybe-debug-log
 maybe-debug-file)
