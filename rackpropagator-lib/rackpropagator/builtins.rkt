#lang racket/base

(require racket/generic
         racket/contract
         racket/vector
         math/array)

(provide
 (struct-out proc-result)
 (rename-out [proc-result-primal primal]
             [proc-result-backprop backprop])

 (struct-out gen-zero)

 zero
 coerce-zero

 car0
 cdr0
 null0?
 foldl0

 make-empty-hasheq

 gen:linear
 linear?
 linear/c
 linear-add
 linear-scale
 linear-dot

 add
 scale
 dot

 unknown-transform
 current-unknown-transform
 error-unknown-transform
 error-unknown-proc-transform
 error-non-zero-sensitivity-transform)

;; ----------------------------------------

(struct proc-result (primal backprop)
  #:transparent)

;; ----------------------------------------

(struct gen-zero ())

(define (zero v) (scale v (gen-zero)))
(define (coerce-zero u v) (add u (zero v)))

(define ((lift-zero f) x)
  (if (gen-zero? x)
      (gen-zero)
      (f x)))

(define (proc-result-primal0 x) ((lift-zero proc-result-primal) x))
(define (proc-result-backprop0 x) ((lift-zero proc-result-backprop) x))
(define (car0 a) ((lift-zero car) a))
(define (cdr0 a) ((lift-zero cdr) a))

(define (null0? x) (or (null? x) (gen-zero? x)))

(define (foldl0 f z xs)
  (if (null0? xs)
      z
      (foldl0 f (f (car xs) z) (cdr xs))))

;; ----------------------------------------

;; like make-hasheq, but no initial assoc argument
(define (make-empty-hasheq) (make-hasheq))

(define (hash-merge h g)
  (define result (hash-copy h))
  (for ([(k v) (in-hash g)])
    (hash-update! result k (λ (u) (add u v)) v))
  result)

(define-generics linear
  (linear-add linear other)
  (linear-scale linear num)
  (linear-dot linear other)

  #:fast-defaults
  ([number?
    (define (linear-add u v) (if (gen-zero? v) u (+ u v)))
    (define (linear-scale u a) (if (gen-zero? a)
                              (- u u) ; zero with the same type as u
                              (* a u)))
    (define (linear-dot u v) (linear-scale u v))]

   [null?
    (define (linear-add u v) null)
    (define (linear-scale u a) null)
    (define (linear-dot u v) null)]

   [gen-zero?
    (define (linear-add u v) v)
    (define (linear-scale u a) (gen-zero))
    (define (linear-dot u v) (if (gen-zero? v)
                                 (gen-zero)
                                 (dot v u)))]

   [vector?
    (define (linear-add u v) (if (gen-zero? v) u (vector-map add u v)))
    (define (linear-scale u a) (vector-map (λ (x) (scale x a)) u))
    (define (linear-dot u v)
      (for/fold ([r (gen-zero)])
                ([e (vector-map dot u (coerce-zero v u))])
        (add r e)))]

   [pair?
    (define (linear-add u v)
      (cons (add (car u) (car0 v))
            (add (cdr u) (cdr0 v))))
    (define (linear-scale u a)
      (cons (scale (car u) a)
            (scale (cdr u) a)))
    (define (linear-dot u v)
      (add (dot (car u) (car0 v))
           (dot (cdr u) (cdr0 v))))]

   [array?
    (define (linear-add u v) (array-map add u v))
    (define (linear-scale u a) (array-map (λ (x) (scale x a)) u))
    (define (linear-dot u v)
      (array-all-fold (array-map scale u (coerce-zero v u))
                      add (gen-zero)))]

   [proc-result?
    (define (linear-add u v)
      (proc-result (add (proc-result-primal u) (proc-result-primal0 v))
                   (add (proc-result-backprop u) (proc-result-backprop0 v))))
    (define (linear-scale u a)
      (proc-result (scale (proc-result-primal u) a)
                   (scale (proc-result-backprop u) a)))
    (define (linear-dot u v)
      (add (dot (proc-result-primal u) (proc-result-primal0 v))
           (dot (proc-result-backprop u) (proc-result-backprop0 v))))]

   [hash?
    (define (linear-add u v) (if (gen-zero? v) u (hash-merge u v)))
    (define (linear-scale u a)
      (for/hash ([(k v) (in-hash u)])
        (values k (scale v a))))
    (define (linear-dot h g)
      (let ([g* (coerce-zero g h)])
        (for/fold ([acc (gen-zero)])
                  ([(k v) (in-hash h)]
                   #:when (hash-has-key? g* k))
          (add (dot v (hash-ref g* k))))))]

   [box?
    (define (linear-add u v)
      (if (gen-zero? v)
          u
          ((if (immutable? u) box-immutable box)
           (add (unbox u) (unbox v)))))
    (define (linear-scale u a)
      ((if (immutable? u) box-immutable box)
       (scale (unbox u) a)))
    (define (linear-dot u v)
      (let ([v* (coerce-zero v u)])
        (dot (unbox u) (unbox v*))))]))

(define (scale u a)
  (cond
    [(linear? u) (linear-scale u a)]
    [(gen-zero? a) (gen-zero)]
    [else (error "can't scale")]))

(define (add u v)
  (cond
    [(linear? u) (linear-add u v)]
    [(gen-zero? v) u]
    [else (error "can't add")]))

(define (dot u v)
  (cond
    [(linear? u) (linear-dot u v)]
    [(gen-zero? v) (gen-zero)]
    [else (error "can't take dot product")]))


;; ----------------------------------------

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
          (proc-result
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
