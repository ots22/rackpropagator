#lang racket/base

;;
;; Primitives available to user code: generic zero and addition
;;

; (require "closure.rkt")

(provide gen-zero
         gen-zero?
         ; zero
         coerce-zero
         zero-car
         zero-cdr
         add)

(define (zero a)
  (cond
    [(null? a) null]
    [(pair? a) (cons (zero (car a)) (zero (cdr a)))]
    [(procedure? a) null]
    [else 0.0]))

(struct gen-zero ())

;; Walk a, and if it contains a gen-zero, coerce this to have a
;; conforming shape to b
;; 
;; coerce-zero : pair? pair? -> pair?

;; TODO better error message for non-conforming a and b
(define (coerce-zero a b)
  (cond
    [(gen-zero? a) (zero b)]
    [(pair? a) (cons (coerce-zero (car a) (car b))
                     (coerce-zero (cdr a) (cdr b)))]
    [else a]))

(define (zero-car a)
  (if (gen-zero? a)
      a
      (car a)))

(define (zero-cdr a)
  (if (gen-zero? a)
      a
      (cdr a)))

;; (define (zero? a)
;;   (equal? a (zero a)))

(define (add a b)
  (cond
    [(gen-zero? a) b]
    [(gen-zero? b) a]
    [(and (null? a) (null? b)) null]
    [(pair? a) (cons (add (car a) (car b))
                     (add (cdr a) (cdr b)))]
    [else (+ a b)]))
