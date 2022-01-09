#lang racket

(require (prefix-in a: math/array)
         (prefix-in m: math/matrix))

(provide matrix* matrix-transpose matrix-scale matrix-inverse list->matrix
         matrix->list matrix-num-rows matrix-num-cols array* array-all-sum)

(define (matrix* A B) (m:matrix* A B))

(define (matrix-transpose A) (m:matrix-transpose A))

(define (matrix-scale M a) (m:matrix-scale M a))

(define (matrix-inverse A) (m:matrix-inverse A))

(define (list->matrix m n xs) (m:list->matrix m n xs))

(define (matrix->list M) (m:matrix->list M))

(define (matrix-num-rows M) (m:matrix-num-rows M))

(define (matrix-num-cols M) (m:matrix-num-cols M))

(define (array* A B) (a:array* A B))

(define (array-all-sum A) (a:array-all-sum A))
