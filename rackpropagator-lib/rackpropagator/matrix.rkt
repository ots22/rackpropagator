#lang racket

(require "matrix-wrappers.rkt"
         "builtins.rkt"
         "derivative.rkt"
         "prim-definition.rkt"
         (for-syntax "builtins.rkt"
                     racket/base
                     racket/list
                     racket/syntax))

(provide (all-from-out "matrix-wrappers.rkt"))

(require/backprop
 "matrix-wrappers.rkt"
 [(matrix* a b)
  (λ (Aw Abox)
    (list '()
          (matrix* Aw (matrix-transpose b))
          (matrix* (matrix-transpose a) Aw)))]

 [(matrix-transpose a)
  (λ (Aw Abox) (list '() (matrix-transpose Aw)))]

 [(matrix-scale M a)
  (λ (Aw Abox)
    (list '() (scale Aw a) (dot Aw M)))]

 [(list->matrix m n a)
  (λ (Aw Abox) (list '() 0 0 (matrix->list Aw)))]

 [(matrix->list M)
  (λ (Aw Abox)
    (let ([m (matrix-num-rows M)]
          [n (matrix-num-cols M)])
      (list '() (list->matrix m n Aw))))]

 [(matrix-num-rows M)
  (λ (Aw Abox) (list '() (gen-zero)))]

 [(matrix-num-cols M)
  (λ (Aw Abox) (list '() (gen-zero)))])

(require/primal+backprop
 "matrix-wrappers.rkt"
 [matrix-inverse
  (λ (M)
    (let ([invM (matrix-inverse M)])
      (proc-result
       invM
       (λ (Aw Abox)
         (let ([invMT (matrix-transpose invM)])
           (list '() (matrix-scale (matrix* (matrix* invMT Aw) invMT)
                                   -1)))))))])

(require/backprop
 "matrix-wrappers.rkt"
 [(array* A B)
  (λ (Aw Abox)
    (let ([Aw* (coerce-zero Aw A)])
      (list '() (array* Aw* B) (array* Aw* A))))])
