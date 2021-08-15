#lang racket/base

;;
;; Primitives available to user code: generic zero and addition
;;

(require "closure.rkt")

(provide zero
         add)

;; TODO
;; make zero lazy (wrap the argument in a struct and compute only if
;; needed (if left at the end)
;;
;; 'add' and a zero can be done efficiently

(define (zero a)
  (cond
    [(null? a) null]
    [(pair? a) (cons (zero (car a)) (zero (cdr a)))]
    [(closure? a) (closure-zero a)]
    [(procedure? a) null]
    [else 0.0]))

(define (zero? a)
  (equal? a (zero a)))

(define (add a b)
  (cond
    [(and (null? a) (null? b)) null]
    [(pair? a) (cons (add (car a) (car b))
                     (add (cdr a) (cdr b)))]
    [else (+ a b)]))
