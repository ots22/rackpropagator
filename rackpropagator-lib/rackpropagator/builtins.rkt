#lang racket/base

(require racket/generic
         racket/vector
         math/array)

(provide (rename-out [make-gen-zero gen-zero]
                     [make-proc-result proc-result]
                     [proc-result proc-result*]
                     [proc-result-primal0 primal]
                     [proc-result-backprop0 backprop])
         proc-result?
         strip-backprop
         unknown-transform
         current-unknown-transform
         error-unknown-transform
         error-unknown-proc-transform
         error-non-zero-sensitivity-transform
         gen-zero?
         zero
         coerce-zero
         car0
         cdr0
         foldl0
         add
         scale)


(struct proc-result (primal backprop)
  #:transparent
  #:constructor-name make-proc-result)

(define (proc-result-primal0 x) ((lift-zero proc-result-primal) x))
(define (proc-result-backprop0 x) ((lift-zero proc-result-backprop) x))


(struct gen-zero () #:constructor-name make-gen-zero)

(define (zero v) (scale v (make-gen-zero)))


(define ((lift-zero f) x)
  (if (gen-zero? x)
      (make-gen-zero)
      (f x)))

(define (car0 a) ((lift-zero car) a))
(define (cdr0 a) ((lift-zero cdr) a))

(define (foldl0 f z xs)
  (if (or (null? xs) (gen-zero? xs))
      z
      (foldl0 f (f z (car xs)) (cdr xs))))


(define-generics linear
  (add linear other)
  (scale linear num)

  ;; coerce the first argument to be a member of the space of the
  ;; second (from a structure that may contain gen-zero)
  (coerce-zero linear other)

  #:fast-defaults
  ([number?
    (define (add u v) (if (gen-zero? v) u (+ u v)))
    (define (scale u a) (if (gen-zero? a) 0.0 (* a u)))
    (define (coerce-zero u v) u)]

   [gen-zero?
    (define (add u v) v)
    (define (scale u a) (make-gen-zero))
    (define (coerce-zero u v) (zero v))]

   [null?
    (define (add u v) null)
    (define (scale u a) null)
    (define (coerce-zero u v) null)]

   [pair?
    (define/generic super-add add)
    (define/generic super-scale scale)
    (define/generic super-coerce-zero coerce-zero)
    (define (add u v)
      (cons (super-add (car u) (car0 v))
            (super-add (cdr u) (cdr0 v))))
    (define (scale u a)
      (cons (super-scale (car u) a)
            (super-scale (cdr u) a)))
    (define (coerce-zero u v)
      (cons (super-coerce-zero (car u) (car0 v))
            (super-coerce-zero (cdr u) (cdr0 v))))]

   [vector?
    (define/generic super-add add)
    (define/generic super-scale scale)
    (define/generic super-coerce-zero coerce-zero)
    (define (add u v) (vector-map super-add u v))
    (define (scale u a) (vector-map (λ (x) (super-scale x a)) u))
    (define (coerce-zero u v) (vector-map super-coerce-zero u v))]

   [array?
    (define/generic super-add add)
    (define/generic super-scale scale)
    (define/generic super-coerce-zero coerce-zero)
    (define (add u v) (array-map super-add u v))
    (define (scale u a) (array-map (λ (x) (super-scale x a)) u))
    (define (coerce-zero u v) (array-map super-coerce-zero u v))]

   [proc-result?
    (define/generic super-add add)
    (define/generic super-scale scale)
    (define/generic super-coerce-zero coerce-zero)
    (define (add u v)
      (make-proc-result (super-add (proc-result-primal u)
                                   (proc-result-primal0 v))
                        (super-add (proc-result-backprop u)
                                   (proc-result-backprop0 v))))
    (define (scale u a)
      (make-proc-result (super-scale (proc-result-primal u) a)
                        (super-scale (proc-result-backprop u) a)))
    (define (coerce-zero u v)
      (make-proc-result (super-coerce-zero (proc-result-primal u)
                                           (proc-result-primal0 v))
                        (super-coerce-zero (proc-result-backprop u)
                                           (proc-result-backprop0 v))))]))


;; ----------------------------------------

(define (strip-backprop p)
  (cond
    [(procedure? p)
     (λ xs (strip-backprop (apply p (map strip-backprop xs))))]
    [(proc-result? p) (proc-result-primal0 p)]
    [else p]))


(define (error-unknown-transform op op-name)
  (raise-arguments-error 'lift/D+
                         "Backpropagator unknown"
                         "op" op-name))

(define (error-unknown-proc-transform op op-name)
  (if (procedure? op)
      (error-unknown-transform op op-name)
      op))

(define (error-non-zero-sensitivity-transform op op-name)
  (if (procedure? op)
      (λ xs
        (let ([result (apply op xs)])
          (make-proc-result
           result
           (λ (Aw Abox)
             (if (gen-zero? Aw)
                 Aw
                 (error-unknown-transform op op-name))))))
      op))

(define current-unknown-transform
  (make-parameter error-non-zero-sensitivity-transform))

(define (unknown-transform op op-name)
  ((current-unknown-transform) op op-name))
