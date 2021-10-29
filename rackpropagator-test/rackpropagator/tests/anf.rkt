#lang racket/base

(require rackunit
         rackpropagator/anf)

(test-case "anf expand"
  (check-true  (anf1? #'(let-values (((a) '1)) (#%plain-app + '1 '2))))
  (check-false (anf1? #'(let ((a (let ((b 1)) b))) a))))

(test-case "anf normalize"
  (check-true
   (with-syntax ([x #'(if '#t (#%plain-app + '1 '2) (#%plain-app + '2 '3))]
                 [y #'(if '#f (#%plain-app + '3 '4) (#%plain-app / '1 '2))])
     (anf1? (anf1-normalize #'(#%plain-app + x y))))))

(test-case "anf normalize 2"
  (define u1 (expand #'(#%plain-app + (let-values (((a) '1)) a) (let-values (((a) '2)) a))))
  (define M1 (anf1-normalize u1))
  (check-true (anf1? M1))
  (check-equal? (eval-syntax M1) 3))

(test-case "anf fib"
  (define fib-stx
    (expand
     #'(let ((fib (lambda (n k)
                    (if (< n 2)
                        1
                        (+ (k (- n 1) k)
                           (k (- n 2) k))))))
         (fib 20 fib))))

  (check-false (anf1? fib-stx))

  (define fib-stx-anf1 (anf1-normalize fib-stx))
  (check-true (anf1? fib-stx-anf1))
  (check-false (anf2? fib-stx-anf1))

  (define fib-stx-anf2 (anf1->2 fib-stx-anf1))
  (check-true (anf2? fib-stx-anf2))

  (check-equal? 10946 (eval-syntax fib-stx-anf2)))

(test-case "anf fib 2"
  (define fib-stx
    (expand
     #'(let ()
         (define (fib n)
           (if (< n 2)
               1
               (+ (fib (- n 1)) (fib (- n 2)))))
         (fib 20))))

  (define fib-stx-anf2 (anf2-normalize fib-stx))

  (check-true (anf2? fib-stx-anf2))
  (check-equal? 10946 (eval-syntax fib-stx-anf2)))

(test-case "anf let/let*/letrec"
  (check-equal?
   (eval-syntax
    (anf2-normalize
     (expand #'(let ([x 0])
                 (let ([x 1]
                       [y x])
                   (+ x y))))))
   1)

  (check-equal?
   (eval-syntax
    (anf2-normalize
     (expand #'(let ([x 0])
                 (let* ([x 1]
                        [y x])
                   (+ x y))))))
   2)

  (check-equal?
   (eval-syntax
    (anf2-normalize
     (expand #'(let ([x (λ () 0)]
                     [y 0])
                 (letrec ([x (λ () (+ y 1))]
                          [y 1])
                   (+ (x) y))))))
   3)
  )

(test-case "if"
  (check-equal?
   (eval-syntax
    (anf2-normalize
     (expand #'(let ((a (if #t (list (list)) #f))) a))))
   '(())))

(test-case "Multiple body forms"
  (check-equal?
   (eval-syntax
    (anf1-normalize
     (expand #'(let ((a (box 1)))
                 (set-box! a (add1 (unbox a)))
                 (unbox a)))))
   2)

  (check-equal?
   (eval-syntax
    (anf1-normalize
     (expand #'((λ () (begin 1 2))))))
   2))
