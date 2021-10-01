#lang racket/base

;;
;; Primitives available to user code: generic zero and addition
;;

; (require "closure.rkt")

(provide (rename-out [make-gen-zero gen-zero]
                     [make-proc-result proc-result]
                     [proc-result proc-result*])
         gen-zero?
         zero
         coerce-zero
         car0
         cdr0
         add
         scale)

(define (zero a)
  (cond
    [(null? a) null]
    [(pair? a) (cons (zero (car a)) (zero (cdr a)))]
    [(proc-result? a) (make-proc-result (zero (proc-result-primal0 a))
                                        (zero (proc-result-backprop0 a)))]
    [(gen-zero? a) (make-gen-zero)]
    [else 0.0]
    ))

(define ((lift-zero f) x) (if (gen-zero? x)
                              (make-gen-zero)
                              (f x)))

(struct gen-zero ()
  #:constructor-name make-gen-zero)

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
    [(proc-result? a)
     (make-proc-result
      (coerce-zero (proc-result-primal0 a) (proc-result-primal0 b))
      (coerce-zero (proc-result-backprop0 a) (proc-result-backprop0 b)))]

    [else a]))

(define (car0 a) ((lift-zero car) a))
(define (cdr0 a) ((lift-zero cdr) a))

;; (define (zero? a)
;;   (equal? a (zero a)))

(define (add a b)
  (cond
    [(gen-zero? a) b]
    [(gen-zero? b) a]
    [(and (null? a) (null? b)) null]
    [(pair? a) (cons (add (car a) (car b))
                     (add (cdr a) (cdr b)))]

    [(proc-result? a)
     (make-proc-result
      (add (proc-result-primal0 a) (proc-result-primal0 b))
      (add (proc-result-backprop0 a) (proc-result-backprop0 b)))]
      
    [else (+ a b)]))

(define (scale a b)
  (cond
    [(gen-zero? a) a]
    [else (* a b)]))

(struct proc-result (primal backprop)
  #:transparent
  #:constructor-name make-proc-result)

(define (proc-result-primal0 x) ((lift-zero proc-result-primal) x))
(define (proc-result-backprop0 x) ((lift-zero proc-result-backprop) x))
