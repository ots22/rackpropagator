#lang racket/base

(require scribble/eval
         scriblib/autobib
         (for-label (except-in racket apply)
                    rackpropagator
                    rackpropagator/anf)
         "bibliography.rkt")

(provide (all-defined-out)
         (all-from-out scribble/eval
                       scriblib/autobib)
         the-eval
         ~cite
         citet
         generate-bibliography
         (for-label (all-from-out racket
                                  rackpropagator
                                  rackpropagator/anf))
         (all-from-out "bibliography.rkt"))

(define the-eval (make-base-eval))
(the-eval '(require rackpropagator))

(define-cite ~cite citet generate-bibliography)
