#lang racket/base

;;
;; Primitives available to user code: generic zero and addition
;;

; (require "closure.rkt")

(provide gen-zero
         gen-zero?
         zero-car
         zero-cdr
         add)

;; TODO
;; make zero lazy (wrap the argument in a struct and compute only if
;; needed (if left at the end)
;;
;; 'add' and a zero can be done efficiently

;; (define (zero a)
;;   (cond
;;     [(null? a) null]
;;     [(pair? a) (cons (zero (car a)) (zero (cdr a)))]
;;     [(closure? a) ((closure-zero a))]
;;     [(procedure? a) null]
;;     [else 0.0]))

(struct gen-zero ())

;; TODO coerce-zero

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
