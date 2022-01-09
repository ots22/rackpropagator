#lang racket/base

(require racket/stxparam
         syntax/parse/define
         "builtins.rkt"
         (for-syntax racket/base
                     racket/dict
                     racket/provide-transform
                     racket/splicing
                     syntax/id-table
                     syntax/parse))

(provide register-primitive!
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

;; Table mapping primitive bindings to the definition of their reverse
;; transformation

(define-for-syntax prim-table (make-free-id-table))


;; ----------------------------------------

(define-for-syntax (get-prim-definition id)
  (dict-ref prim-table id #f))

(define-for-syntax (set-prim-definition! prim-id prim-augmented-def)
  (dict-set! prim-table prim-id prim-augmented-def))


(define-syntax (register-primitive! stx)
  (syntax-parse stx
    [(_ prim-id prim-augmented-def)
     ;; expand to handle *some* impersonators, renaming transformers etc
     #:with prim-id* (local-expand #'prim-id 'expression '())
     #:with prim-augmented-def* (local-expand #'prim-augmented-def 'expression '())
     #'(begin-for-syntax
         (set-prim-definition! #'prim-id* #'prim-augmented-def*))]))


(define-syntax-parse-rule (require/primal+backprop
                           path
                           [prim-id prim-augmented-def] ...)
  (begin
    (require (only-in path prim-id ...))
    (register-primitive! prim-id prim-augmented-def) ...))

(define-syntax-parse-rule (require/backprop
                           path
                           [prim-spec:prim-spec backprop-def:expr] ...)
  (require/primal+backprop
   path
   [prim-spec.prim-id
    (λ prim-spec.vars (proc-result prim-spec.appl backprop-def))] ...))
