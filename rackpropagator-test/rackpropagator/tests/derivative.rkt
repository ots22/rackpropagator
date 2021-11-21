#lang racket/base

(require racket/match
         syntax/macro-testing
         syntax/parse/define
         rackunit
         rackpropagator
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
    (match-let ([(proc-result* p b) (apply Df args)])
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
                    #:derivative '(12.0 0.0)))

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
                    #:derivative '(12.0 0.0)))

(test-case "pow"
  (check-derivative (λ (x n)
                      (define (pow x n)
                        (if (= n 0) 1.0 (* x (pow x (- n 1)))))
                      (pow x n))
                    #:at '(2.0 3)
                    #:primal 8.0
                    #:derivative '(12.0 0.0)))

(test-case "pow 2"
  (check-derivative (λ (x n)
                      (define (pow x n r)
                        (if (= n 0) r (pow x (- n 1) (* r x))))
                      (pow x n 1))
                    #:at '(2.0 3)
                    #:primal 8.0
                    #:derivative '(12.0 0.0)))

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
                    #:derivative '(585.0 0.0)))

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
                    #:derivative '(12.0 0.0))

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
                    #:derivative '(12.0 0.0))
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

                    
