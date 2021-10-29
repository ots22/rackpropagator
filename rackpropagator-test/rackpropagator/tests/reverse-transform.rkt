#lang racket/base

(require rackunit
         rackpropagator/reverse-transform)

(check-not-exn
 (λ ()
   (reverse-transform
    #'(#%plain-lambda (x)
        (let-values (((result) (#%plain-app + x x)))
          result)))))
