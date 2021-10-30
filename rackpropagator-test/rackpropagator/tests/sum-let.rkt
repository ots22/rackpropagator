#lang racket/base

(require
 syntax/macro-testing
 rackunit
 rackpropagator/sum-let)

(check-equal?
 (sum-let* ([a 1]
            [b 2]
            [c 3]
            [a b]
            [c b]
            [a c])
           (list a b c))

 (let* ([a 1]
        [b 2]
        [c 3]
        [a (+ a b)]
        [c (+ c b)]
        [a (+ a c)])
   (list a b c)))

(check-exn
 exn:fail?
 (λ ()
   (convert-compile-time-error
    (sum-let* ([a 1]
               ;; can't refer to 'a' here, since its definition is not
               ;; complete
               [b a]
               [a 2])
              (list a b)))))

(check-equal?
 (destructuring-sum-let* ([(x x) '(1 1)]) x)
 2)

(check-equal?
 (destructuring-sum-let* ([(a b c) '(1 2 3)]
                          [(d e . e) '(100 500 . 500)]
                          [((a b) c ()) (list (list d d) e)])
                         (list a b c))
 '(101 102 1003))
