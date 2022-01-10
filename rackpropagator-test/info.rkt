#lang info

(define collection 'multi)

(define deps '("base"))

(define build-deps '("rackpropagator-lib"
                     "rackunit-lib"))

(define update-implies '("rackpropagator-lib"))

(define pkg-desc "Rackpropagator tests")

(define pkg-authors '(ots22))
