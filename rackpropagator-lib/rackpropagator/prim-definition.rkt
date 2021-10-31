#lang racket/base

(require racket/stxparam
         (for-syntax racket/base
                     racket/dict
                     racket/provide-transform
                     racket/splicing
                     syntax/id-table
                     syntax/parse))

(provide backprop-out
         require/backprop
         current-box-adjoints
         (for-syntax prim-definition))

(begin-for-syntax
  (define prim-table (make-free-id-table)))

(define-syntax-parameter current-box-adjoints #f)

(define-for-syntax (register-primitive! prim prim-augmented-def)
  (dict-set! prim-table prim prim-augmented-def))

(define-for-syntax (prim-definition a)
  (dict-ref prim-table a))

(define-syntax backprop-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ p ...)
        (begin
          (map syntax-local-lift-module-end-declaration
               (syntax-e #'((register-primitive! p) ...)))
          (pre-expand-export #'(combine-out p ...) modes))]))))

(define-syntax (require/backprop stx)
  (syntax-parse stx
    [(_ path [name name-augmented-def] ...)
     #'(begin
         (require (only-in path name ...))
         (begin-for-syntax
           (register-primitive! #'name #'name-augmented-def) ...))]))
