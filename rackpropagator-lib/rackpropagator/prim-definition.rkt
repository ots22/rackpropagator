#lang racket/base

(require racket/stxparam
         syntax/parse/define
         "builtins.rkt"
         "apply.rkt"
         (for-syntax (except-in racket/base apply)
                     "apply.rkt"
                     racket/dict
                     racket/provide-transform
                     racket/splicing
                     syntax/id-table
                     syntax/parse))

(provide local-register-primitive!
         register-primitive!
         backprop-out
         require/primal+backprop
         require/backprop
         (for-syntax prim-spec
                     get-prim-definition
                     set-prim-definition!))


(begin-for-syntax
  (define-syntax-class prim-spec
    (pattern (prim-id:id xs:id ...)
             #:attr vars #'(xs ...)
             #:attr appl #'(prim-id xs ...))
    (pattern (prim-id:id . xs*:id)
             #:attr vars #'xs*
             #:attr appl #'(apply prim-id xs*))
    (pattern (prim-id:id xs:id ...+ . xs*:id)
             #:attr vars #'(xs ... . xs*)
             #:attr appl #'(apply prim-id xs ... xs*))))

;; ----------------------------------------

(define-for-syntax prim-table (make-free-id-table))

(define-for-syntax (get-prim-definition id)
  (dict-ref prim-table id #f))

(define-for-syntax (set-prim-definition! prim-id prim-augmented-def)
  (dict-set! prim-table prim-id prim-augmented-def))

;; ----------------------------------------

(define-syntax (local-register-primitive! stx)
  (syntax-parse stx
    [(_ prim-id prim-augmented-def)
     (set-prim-definition! #'prim-id #'prim-augmented-def)
     #'(void)]))

(define-syntax (register-primitive! stx)
  (syntax-parse stx
    [(_ prim-id)
     #:with prim-augmented-def (get-prim-definition #'prim-id)
     #'(begin-for-syntax
         (set-prim-definition! #'prim-id #'prim-augmented-def))]))

;; ----------------------------------------

(define-syntax backprop-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ p ...)
        (begin
          (map syntax-local-lift-module-end-declaration
               (syntax-e #'((register-primitive! p) ...)))
          (pre-expand-export #'(combine-out p ...) modes))]))))

;; ----------------------------------------

(define-syntax-parse-rule (require/primal+backprop
                           path
                           [prim-id prim-augmented-def] ...)
  (begin
    (require (only-in path prim-id ...))
    (local-register-primitive! prim-id prim-augmented-def) ...))

(define-syntax-parse-rule (require/backprop
                           path
                           [prim-spec:prim-spec backprop-def:expr] ...)
  (require/primal+backprop
   path
   [prim-spec.prim-id
    (λ prim-spec.vars (proc-result prim-spec.appl backprop-def))] ...))
