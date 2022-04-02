#lang info

(define collection 'multi)

(define deps '("base"))

(define build-deps '("racket-doc"
                     "math-doc"
                     "math-lib"
                     "scribble-lib"
                     "scribble-math"
                     "rackpropagator-lib"))

(define update-implies '("rackpropagator-lib"))

(define pkg-desc "Rackpropagator documentation")

(define pkg-authors '(ots22))
