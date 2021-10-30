#lang racket/base

(require syntax/parse
         (for-syntax racket/base
                     syntax/parse
                     syntax/stx))

(provide all-equal?
         (for-syntax stx-caar
                     stx-cadr)
         pattern-lambda
         pat-λ
         syntax-class->predicate)

(define (all-equal? . xs)
  (if (null? xs)
      #t
      (andmap (λ (x) (equal? x (car xs))) (cdr xs))))

(define-for-syntax stx-caar (compose1 stx-car stx-car))
(define-for-syntax stx-cadr (compose1 stx-car stx-cdr))

;; like lambda, except all arguments are treated as pattern variables
;; for constructing a syntax object
(define-syntax pattern-lambda
  (syntax-parser
    [(_ (args ...) body)
     #'(lambda (args ...)
         (with-syntax ([args args] ...)
           body))]))

(define-syntax pat-λ (make-rename-transformer #'pattern-lambda))

(define-syntax-rule (syntax-class->predicate stx-class)
  (syntax-parser
    [(~var _ stx-class) #t]
    [_ #f]))
