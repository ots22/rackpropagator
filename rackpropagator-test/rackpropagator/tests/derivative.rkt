#lang racket/base

(require racket/match
         syntax/macro-testing
         rackunit
         rackpropagator)

(test-case "plus"
  (check-not-exn
   (λ ()
     (convert-compile-time-error
      (D+ (λ (x) (+ x x))))))

  (check-not-exn
   (λ ()
     (convert-compile-time-error
      (D+ (λ (x) (let ([f +])
                   (f x x))))))))

(test-case "closure"
  (check-not-exn
   (λ ()
     (convert-compile-time-error
      (let ((y 1))
        (D+ (lambda (x)
              (let ((result (+ x y)))
                result))))))))

(test-case "identity"
  (match-let* ([D+f (D+ (λ (a)
                          (((λ (b)
                              (λ (c)
                                b)) a) 1)))]
               [(proc-result* y <-y) (D+f 184.0)])
    (check-equal? y 184.0)
    (check-equal? (<-y 1.0) '(1.0))))

(test-case "conditional-1"
  (match-let* ([D+f (D+ (λ (a) (if #t a a)))]
               [(proc-result* primal1 backprop1) (D+f 5.0)]
               [(proc-result* primal2 backprop2) (D+f 15.0)])
    (check-equal? primal1 5.0)
    (check-equal? primal2 15.0)
    (check-equal? (backprop1 1.0) '(1.0))
    (check-equal? (backprop2 1.0) '(1.0))))

(test-case "conditional"
  (match-let* ([D+f (D+ (λ (a) (if (> a 10) a 0.0)))]
               [(proc-result* primal1 backprop1) (D+f 5.0)]
               [(proc-result* primal2 backprop2) (D+f 15.0)])
    (check-equal? primal1 0.0)
    (check-equal? primal2 15.0)
    (check-equal? (backprop1 1.0) '(0.0))
    (check-equal? (backprop2 1.0) '(1.0))))

(test-case "pow (Y)"
  (define D+pow
    (D+ (λ (x n)
          (let* ([pow* (λ (x n rec)
                         (if (= n 0)
                             1.0
                             (* x (rec x (- n 1) rec))))]
                 [pow (λ (x n) (pow* x n pow*))])
            (pow x n)))))
  (check-equal? ((backprop (D+pow 2.0 3.0)) 1.0)
                '(12.0 0.0)))

(test-case "pow (box)"
  (define D+pow
    (D+ (λ (x n)
          (let ([f (box '())])
            (set-box! f (λ (x n)
                          (if (= n 0)
                              1.0
                              (* x ((unbox f) x (- n 1))))))
            ((unbox f) x n)))))
  (check-equal? (primal (D+pow 2.0 3)) 8.0)
  (check-equal? ((backprop (D+pow 2.0 3)) 1.0)
                '(12.0 0.0)))

(test-case "pow"
  (define D+pow
    (D+ (λ (x n)
          (define (pow x n) (if (= n 0) 1.0 (* x (pow x (- n 1)))))
          (pow x n))))

  (check-equal? ((backprop (D+pow 2.0 3.0)) 1.0)
                '(12.0 0.0)))

(test-case "pow 2"
  (define D+pow
    (D+ (λ (x n)
          (define (pow x n r) (if (= n 0) r (pow x (- n 1) (* r x))))
          (pow x n 1))))

  (check-equal? ((backprop (D+pow 2.0 3.0)) 1.0)
                '(12.0 0.0)))

(test-case "scale gen-zero"
  (define D+pow
    (D+ (λ (x)
          ;; f accumulates a result into r, but do not use it. Check
          ;; that the backpropagator can handle scaling by the
          ;; resulting gen-zero sensitivity.
          (define (f x r) (if (< x 0) 1 (f (- x 1) (* r x))))
          (f x 1))))

  (check-equal? ((backprop (D+pow 2.0)) 1.0)
                '(0.0)))

(test-case "Mutual recursion"
  (define D+fn
    (D+
     (λ (x n)
       (letrec ([f (λ (x n) (if (= n 0)
                                x
                                (* x (g x (- n 1)))))]
                [g (λ (x n) (if (= n 0)
                                x
                                (+ x (f x (- n 1)))))])
         (f x n)))))

  (match-let ([(proc-result* y <-y) (D+fn 5 5)])
    (check-equal? y 775)
    (check-equal? (<-y 1.0) '(585.0 0.0))))

(test-case "Different final use"
  (check-equal?
   ((backprop ((D+ (λ (x) (let ([b 1]) x))) 2)) 1)
   '(1))

  (let ([D+f (let ([a 1]) (D+ (λ (x) (let ([b a]) x))))])
    (check-equal?
     ((backprop (D+f 2)) 1)
     '(1))))

(test-case "list/cons/car/cdr"
  (check-equal?
   ((backprop ((D+ (λ (x y) (list (car x) (cdr x) y x y)))
               '(1 2) 3))
    '(1 1 0 (0 . 0) 0))
   '((1 . 1) 0)))

(test-case "lambda formals"
  (check-equal?
   ((backprop ((D+ (λ xs (car xs))) 2.0 3.0)) 1.0)
   '(1.0 0.0))

  (check-equal?
   ((backprop ((D+ (λ (x . xs) (car xs))) 2.0 3.0 4.0)) 1.0)
   '(0.0 1.0 0.0)))

(test-case "map"
  (let ([result ((D+ (λ (xs ys) (map * xs ys))) '(1 2 3) '(4 5 6))])
    (check-equal? (primal result) '(4 10 18))
    (check-equal? ((backprop result) '(-1.0 1.0 0.5))
                  '((-4.0 5.0 3.0) (-1.0 2.0 1.5)))))

(test-case "Primitive binding"
  (define * +)
  (check-exn
   exn:fail?
   (λ () ((backprop ((D+ (λ (x) (* x x))) 1.0)) 1.0))))

(test-case "Second derivative"
  (check-equal?
   ((backprop ((D+ (λ (y) ((backprop ((D+ (λ (x) (* x x))) y)) 1.0))) 5.0)) '(1.0))
   '(2.0))

  (check-equal?
   ((backprop ((D+ (λ (y) ((backprop ((D+ (λ (x) (if (> x 0) (* x x) (- x x)))) y)) 1.0))) 5.0)) '(1.0))
   '(2.0)))

(test-case "boxes"
  (check-equal?
   ((backprop ((D+ (λ (x)
                     (let* ([b (box (* x x))]
                            [__ (set-box! b (* (unbox b) (unbox b)))])
                       (unbox b)))) 3.0)) 1.0)
   '(108.0))


  (check-equal?
   ((backprop ((D+ (λ (x) (unbox (box x)))) 2.0)) 1.0)
   '(1.0))

  ;; boxes are handled specially -- the following might be unexpected!
  (check-equal?
   ((backprop ((D+ (λ (x) (box x))) 5.0)) (box 1.0))
   '(0.0))

  ;; the input sensitivity is completely ignored, in fact
  (check-equal?
   ((backprop ((D+ (λ (x) (box x))) 5.0)) 'ignored)
   '(0.0))

  (check-equal?
   ((backprop ((D+ (λ (x)
                     (let ([w (box x)])
                       (define (f y) (set-box! w (* (unbox w) y)))
                       (let* ([__ (f x)]
                              [__ (f x)])
                         (unbox w))))) 2)) 1)
   '(12))

  (check-equal?
   ((backprop ((D+ (λ (x n)
                     (let ([f (box '())])
                       (set-box! f
                                 (λ (x n)
                                   (if (= n 0)
                                       1.0
                                       (* x ((unbox f) x (- n 1))))))
                       ((unbox f) x n)))) 2.0 3)) 1.0)
   '(12.0 0.0))

  ;; The following should expand to something similar to the example
  ;; above
  (check-equal?
   ((backprop ((D+ (λ (x n)
                     (let ([f '()])
                       (set! f
                             (λ (x n)
                               (if (= n 0)
                                   1.0
                                   (* x (f x (- n 1))))))
                       (f x n)))) 2.0 3)) 1.0)
   '(12.0 0.0))
  ;
  )


;; The expansion of match-let includes a function that has an
;; unknown backpropagator -- in fact, an internal binding that we
;; can't easily provide one for at all.  This function is used only
;; in an error path, so the following should work (thanks to
;; the final case)
;;
(test-case "match-let"
  (define Df (D+ (λ (x) (match-let ([(list a b) x]) (+ a b)))))
  (match-define (proc-result* y <-y) (Df '(1 2)))
  (check-equal? y 3)
  (check-equal? (<-y 1) '((1 1))))
