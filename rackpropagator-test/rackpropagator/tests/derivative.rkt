#lang racket/base

(require racket/match
         racket/contract
         syntax/macro-testing
         syntax/parse/define
         rackunit
         rackpropagator
         rackpropagator/prim-definition
         (for-syntax racket/base
                     syntax/parse))

(define-syntax-parse-rule
  (check-derivative lambda-expr
                    {~seq
                     #:at args
                     {~alt
                      {~once {~seq #:primal primal-expected}}
                      {~once {~seq #:derivative deriv-expected}}
                      {~once {~optional {~seq #:adjoint adjoint}}}} ...} ...)

  #:with (adjoint* ...) #'({~? adjoint 1.0} ...)
  (let ([Df (D+ lambda-expr)])
    (match-let ([(proc-result p b) (apply Df args)])
      (check-equal? p primal-expected)
      (check-equal? (b adjoint*) deriv-expected)) ...))

(test-case "plus"
  (check-derivative (λ (x) (+ x x))
                    #:at '(3.0)
                    #:primal 6.0
                    #:derivative '(2.0))

  (check-derivative (λ (x) (let ([f +])
                             (f x x)))
                    #:at '(3.0)
                    #:primal 6.0
                    #:derivative '(2.0)))

(test-case "closure"
  (let ([y 1.0])
    (check-derivative (λ (x) (+ x y))
                      #:at '(2.0)
                      #:primal 3.0
                      #:derivative '(1.0))))

(test-case "identity"
  (check-derivative (λ (a) (((λ (b) (λ (c) b)) a) 1))
                    #:at '(184.0)
                    #:primal 184.0
                    #:derivative '(1.0)))

(test-case "conditional"
  (check-derivative (λ (a) (if #t a a))
                    #:at '(5.0)
                    #:primal 5.0
                    #:derivative '(1.0)

                    #:at '(15.0)
                    #:primal 15.0
                    #:derivative '(1.0))

  (check-derivative (λ (a) (if (> a 10) a 0.0))
                    #:at '(5.0)
                    #:primal 0.0
                    #:derivative '(0.0)

                    #:at '(15.0)
                    #:primal 15.0
                    #:derivative '(1.0)))

(test-case "pow Y"
  (check-derivative (λ (x n)
                      (let* ([pow* (λ (x n rec)
                                     (if (= n 0)
                                         1.0
                                         (* x (rec x (- n 1) rec))))]
                             [pow (λ (x n) (pow* x n pow*))])
                        (pow x n)))
                    #:at '(2.0 3)
                    #:primal 8.0
                    #:derivative '(12.0 0)))

(test-case "pow (box)"
  (check-derivative (λ (x n)
                      (let ([f (box '())])
                        (set-box! f (λ (x n)
                                      (if (= n 0)
                                          1.0
                                          (* x ((unbox f) x (- n 1))))))
                        ((unbox f) x n)))
                    #:at '(2.0 3)
                    #:primal 8.0
                    #:derivative '(12.0 0)))

(test-case "pow"
  (check-derivative (λ (x n)
                      (define (pow x n)
                        (if (= n 0) 1.0 (* x (pow x (- n 1)))))
                      (pow x n))
                    #:at '(2.0 3)
                    #:primal 8.0
                    #:derivative '(12.0 0)))

(test-case "pow 2"
  (check-derivative (λ (x n)
                      (define (pow x n r)
                        (if (= n 0) r (pow x (- n 1) (* r x))))
                      (pow x n 1))
                    #:at '(2.0 3)
                    #:primal 8.0
                    #:derivative '(12.0 0)))

(test-case "pow (for/fold)"
  (check-derivative (λ (x n)
                      (for/fold ([acc 1.0])
                                ([i (in-range n)])
                        (* acc x)))
                    #:at '(2.0 3)
                    #:primal 8.0
                    #:derivative '(12.0 0)))

(test-case "scale gen-zero"
  (check-derivative (λ (x)
                      ;; f accumulates a result into r, but does not
                      ;; use it. Check that the backpropagator can
                      ;; handle scaling by the resulting gen-zero
                      ;; sensitivity.
                      (define (f x r)
                        (if (< x 0.0) 1.0 (f (- x 1.0) (* r x))))
                      (f x 1.0))
                    #:at '(2.0)
                    #:primal 1.0
                    #:derivative '(0.0)))

(test-case "Mutual recursion"
  (check-derivative (λ (x n)
                      (letrec ([f (λ (x n) (if (= n 0)
                                               x
                                               (* x (g x (- n 1)))))]
                               [g (λ (x n) (if (= n 0)
                                               x
                                               (+ x (f x (- n 1)))))])
                        (f x n)))
                    #:at '(5.0 5)
                    #:primal 775.0
                    #:derivative '(585.0 0)))

(test-case "Different final use"
  (check-derivative (λ (x) (let ([b 1]) x))
                    #:at '(2.0)
                    #:primal 2.0
                    #:derivative '(1.0))

  (let ([a 1])
    (check-derivative (λ (x) (let ([b a]) x))
                      #:at '(2.0)
                      #:primal 2.0
                      #:derivative '(1.0))))

(test-case "list/cons/car/cdr"
  (check-derivative (λ (x y) (list (car x) (cdr x) y x y))
                    #:at '((1.0 . 2.0) 3.0)
                    #:primal '(1.0 2.0 3.0 (1.0 . 2.0) 3.0)
                    #:adjoint '(1.0 1.0 0.0 (0.0 . 0.0) 0.0)
                    #:derivative '((1.0 . 1.0) 0.0)))

(test-case "lambda formals"
  (check-derivative (λ xs (car xs))
                    #:at '(2.0 3.0)
                    #:primal 2.0
                    #:derivative '(1.0 0.0))

  (check-derivative (λ (x . xs) (car xs))
                    #:at '(2.0 3.0 4.0)
                    #:primal 3.0
                    #:derivative '(0.0 1.0 0.0)))

(test-case "map"
  (check-derivative (λ (xs ys) (map * xs ys))
                    #:at '((1.0 2.0 3.0) (4.0 5.0 6.0))
                    #:primal '(4.0 10.0 18.0)
                    #:adjoint '(-1.0 1.0 0.5)
                    #:derivative '((-4.0 5.0 3.0) (-1.0 2.0 1.5))))

(test-case "boxes"
  (check-derivative (λ (x)
                      (let* ([b (box (* x x))])
                        (set-box! b (* (unbox b) (unbox b)))
                        (unbox b)))
                    #:at '(3.0)
                    #:primal 81.0
                    #:derivative '(108.0))

  (check-derivative (λ (x) (unbox (box x)))
                    #:at '(2.0)
                    #:primal 2.0
                    #:derivative '(1.0))

  ;; boxes are handled specially -- the following might be unexpected!
  (check-derivative (λ (x) (box x))
                    #:at '(5.0)
                    #:primal (box 5.0)
                    #:adjoint (box 1.0)
                    #:derivative '(0.0))

  ;; the input sensitivity is completely ignored, in fact
  (check-derivative (λ (x) (box x))
                    #:at '(5.0)
                    #:primal (box 5.0)
                    #:adjoint 'ignored
                    #:derivative '(0.0))

  (check-derivative (λ (x)
                      (let ([w (box x)])
                        (define (mult-w! y) (set-box! w (* (unbox w) y)))
                        (mult-w! x)
                        (mult-w! x)
                        (unbox w)))
                    #:at '(2.0)
                    #:primal '8.0
                    #:derivative '(12.0))

  (check-derivative (λ (x n)
                      (let ([f (box '())])
                        (set-box! f
                                  (λ (x n)
                                    (if (= n 0)
                                        1.0
                                        (* x ((unbox f) x (- n 1))))))
                        ((unbox f) x n)))
                    #:at '(2.0 3)
                    #:primal 8.0
                    #:derivative '(12.0 0))

  ;; The uses of set! in the following should expand to something
  ;; similar to the example above
  (check-derivative (λ (x n)
                      (let ([f '()])
                        (set! f
                              (λ (x n)
                                (if (= n 0)
                                    1.0
                                    (* x (f x (- n 1))))))
                        (f x n)))
                    #:at '(2.0 3)
                    #:primal 8.0
                    #:derivative '(12.0 0))
  ;
  )

;; The expansion of match-let includes a function that has an
;; unknown backpropagator -- in fact, an internal binding that we
;; can't easily provide one for at all.  This function is used only
;; in an error path, so the following will give the expected result
;;
(test-case "match-let"
  (check-derivative (λ (x) (match-let ([(list a b) x]) (+ a b)))
                    #:at '((1.0 2.0))
                    #:primal 3.0
                    #:derivative '((1.0 1.0))))

(test-case "Primitive binding"
  (define * +)
  (check-exn
   exn:fail?
   (λ () ((backprop ((D+ (λ (x) (* x x))) 1.0)) 1.0))))

(test-case "Second derivative"
  (check-equal?
   (((D (grad *)) 3.0 4.0) '(1.0 0.0))
   '(0.0 1.0))

  (check-equal?
   ((backprop
     ((D+ (λ (y) ((backprop ((D+ (λ (x) (* x x)))
                             y))
                  1.0)))
      5.0))
    '(1.0))
   '(2.0))

  (check-equal?
   ((backprop
     ((D+ (λ (y) ((backprop ((D+ (λ (x) (if (> x 0) (* x x) (- x x))))
                             y))
                  1.0)))
      5.0))
    '(1.0))
   '(2.0)))

(test-case "Prim conditional"
  (check-derivative (λ (x) (if * x (- x)))
                    #:at '(5.0)
                    #:primal 5.0
                    #:derivative '(1.0)))

(test-case "Mutation"
  (check-derivative (λ (x) (let ([y x]) (set! y (* y y)) y))
                    #:at '(3.0)
                    #:primal 9.0
                    #:derivative '(6.0))

  (check-derivative (λ (x) (set! x (* x x)) x)
                    #:at '(3.0)
                    #:primal 9.0
                    #:derivative '(6.0))

  ;; the boxed versions of y and z will compare equal? in this
  ;; example, so check that these are distinguished as hash keys with
  ;; eq? instead (incorrect result otherwise).
  (check-derivative (λ (x)
                      (let ([y x]
                            [z x])
                        (set! y (+ y y))
                        (set! z (+ z z))
                        (+ y z)))
                    #:at '(2.0)
                    #:primal 8.0
                    #:derivative '(4.0))

  (check-derivative (λ (x)
                      (let ([f '()])
                        (set! f (λ (y)
                                  (set! x (+ x 1))
                                  (set! y (* x y))
                                  (set! x 100.0)
                                  y))
                        (f x)))
                    #:at '(3.0)
                    #:primal 12.0
                    #:derivative '(7.0))

  (check-derivative (λ (x)
                      (begin
                        (set! x (* 2.0 x))
                        x))
                    #:at '(3.0)
                    #:primal 6.0
                    #:derivative '(2.0))

  (check-derivative (λ (x)
                      (begin0
                          x
                        (set! x (* 2.0 x))))
                    #:at '(3.0)
                    #:primal 3.0
                    #:derivative '(1.0)))

(test-case "lift/D+"
  (check-derivative foldl
                    #:at (list (lift/D+ *) 1 '(2 3 4))
                    #:primal 24
                    #:derivative '(() 24.0 (12.0 8.0 6.0)))

  (check-equal?
   ((grad apply) (let ([y 5.0]) (lift/D+ (λ (x) (* x y)))) (list 2.0))
   '((2.0) (5.0))))

(test-case "Vector"
  (check-derivative (λ xs
                      (vector->list
                       (vector-copy
                        (vector->immutable-vector
                         (list->vector xs)))))
                    #:at '(10 20 30 40)
                    #:primal '(10 20 30 40)
                    #:adjoint '(0.1 0.2 0.3 0.4)
                    #:derivative '(0.1 0.2 0.3 0.4))

  (check-derivative (λ xs
                      (vector->list
                       (vector-copy
                         (list->vector xs))))
                    #:at '(10 20 30 40)
                    #:primal '(10 20 30 40)
                    #:adjoint '(0.1 0.2 0.3 0.4)
                    #:derivative '(0.1 0.2 0.3 0.4))

  (check-derivative (λ xs
                      (define v (list->vector xs))
                      (define v* (vector-copy v))
                      (vector-set! v* 0 (* (vector-ref v* 0) (vector-ref v* 1)))
                      (+ (* (vector-ref v 0) (vector-ref v* 0))
                         (* (vector-ref v 1) (vector-ref v* 1))))
                    #:at '(3 5)
                    #:primal 70
                    #:adjoint 1
                    #:derivative '(30 19))

  (check-derivative (λ xs
                      (define v (vector->immutable-vector (list->vector xs)))
                      (define v* (vector-copy v))
                      (vector-set! v* 0 (* (vector-ref v* 0) (vector-ref v* 1)))
                      (+ (* (vector-ref v 0) (vector-ref v* 0))
                         (* (vector-ref v 1) (vector-ref v* 1))))
                    #:at '(3 5)
                    #:primal 70
                    #:adjoint 1
                    #:derivative '(30 19))

  (check-derivative (λ xs
                      (define v (list->vector xs))
                      (define v* (vector-copy v))
                      (define v** (vector->immutable-vector v))
                      (vector-set! v* 0 (* (vector-ref v* 0) (vector-ref v* 1)))
                      (+ (* (vector-ref v 0) (vector-ref v* 0))
                         (* (vector-ref v 1) (vector-ref v* 1))))
                    #:at '(3 5)
                    #:primal 70
                    #:adjoint 1
                    #:derivative '(30 19)))

(test-case "Vector 2"
  (let ([a 10])
    (check-equal?
     (((D (λ (n proc)
            (vector->immutable-vector
             (build-vector n proc))))
       3 (lift/D+ (λ (x) (* x a))))
      #(0 0 1))
     '(0 (2)))))


(define/contract (square-real x)
  (-> real? real?)
  (* x x))

(register-primitive! square-real
                     (λ (x)
                       (proc-result (square-real x)
                                    (λ (Aw Abox)
                                      (list '() (scale Aw (* 2 x)))))))

(test-case "Contract"
  (check-derivative square-real
                    #:at '(3.0)
                    #:primal 9.0
                    #:derivative '(6.0))

  (check-exn exn:fail:contract?
             (λ () ((grad square-real) 0+i))))

(test-case "Hash table"
  (check-derivative (λ (x)
                      (define h (make-empty-hasheq))
                      (hash-set! h 'b 10)
                      (hash-ref! h 'c (* x 2))
                      (hash-set! h 'a x)
                      (hash-ref h 'a)
                      (hash-ref! h 'a 'd))
                    #:at '(5.0)
                    #:primal 5.0
                    #:derivative '(1.0)))
