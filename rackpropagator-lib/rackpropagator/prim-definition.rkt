#lang racket/base

(require racket/stxparam
         (only-in "builtins.rkt" proc-result)
         (for-syntax racket/base
                     racket/dict
                     racket/provide-transform
                     racket/splicing
                     syntax/id-table
                     syntax/parse))

(provide current-box-adjoints
         current-non-prim-transform
         local-register-primitive!
         register-primitive!
         prim-definition
         backprop-out
         require/primal+backprop
         require/backprop)

(define-for-syntax prim-table (make-free-id-table))

(define-syntax-parameter current-box-adjoints #f)

(define-syntax-parameter current-non-prim-transform #f)

(define-syntax (local-register-primitive! stx)
  (syntax-parse stx
    [(_ prim-id prim-augmented-def)
     (dict-set! prim-table #'prim-id #'prim-augmented-def)
     #'(void)]))

(define-syntax (register-primitive! stx)
  (syntax-parse stx
    [(_ prim-id)
     #:with prim-augmented-def (dict-ref prim-table #'prim-id)
     #'(begin-for-syntax
         (dict-set! prim-table #'prim-id #'prim-augmented-def))]))

(define-syntax (prim-definition stx)
  (syntax-parse stx
    [(_ a) (dict-ref prim-table #'a #'(current-non-prim-transform a))]))

(define-syntax backprop-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ p ...)
        (begin
          (map syntax-local-lift-module-end-declaration
               (syntax-e #'((register-primitive! p) ...)))
          (pre-expand-export #'(combine-out p ...) modes))]))))

(define-syntax-rule (require/primal+backprop path
                                             [prim-id prim-augmented-def] ...)
  (begin
    (require (only-in path prim-id ...))
    (local-register-primitive! prim-id prim-augmented-def) ...))

(define-syntax-parse-rule (require/backprop path [(prim-id . xs:formals) backprop-def] ...)
  (require/primal+backprop
   path
   [prim-id
    (λ xs (proc-result (apply prim-id xs) backprop-def))] ...))
