#lang racket/base

(require scribble/examples
         scribble/html-properties
         scribble/core
         scriblib/autobib
         racket/format
         racket/string
         racket/list
         scribble/racket
         (for-label racket
                    (only-in math/array array)
                    rackpropagator
                    rackpropagator/builtins
                    rackpropagator/matrix
                    rackpropagator/primitives
                    rackpropagator/prim-definition
                    rackpropagator/anf)
         "bibliography.rkt")

(provide (all-defined-out)
         (all-from-out scribble/examples
                       scribble/html-properties
                       scribble/core
                       scriblib/autobib
                       racket/format
                       racket/string
                       racket/list
                       scribble/racket)
         the-eval
         ~cite
         citet
         generate-bibliography
         exported-primitives
         (for-label (all-from-out racket
                                  math/array
                                  rackpropagator
                                  rackpropagator/builtins
                                  rackpropagator/matrix
                                  rackpropagator/primitives
                                  rackpropagator/prim-definition
                                  rackpropagator/anf))
         (all-from-out "bibliography.rkt"))

(define the-eval (make-base-eval))
(the-eval '(begin (require racket/list)
                  (require rackpropagator)
                  (require rackpropagator/prim-definition)
                  (require racket/math)))
          

(define-cite ~cite citet generate-bibliography)

(define-values (exported-primitives exported-syntax-ignored)
  (module->exports 'rackpropagator/primitives))


(define (chunk n fill lst)
  (define-values (nchunk nfinal) (quotient/remainder (length lst) n))
  (define nfill (- n nfinal))
  (for/fold ([acc '()]
             [lst lst]
             #:result (reverse
                       (cons (append lst (make-list nfill fill))
                             acc)))
            ([idx (in-range nchunk)])
    (define-values (head tail) (split-at lst n))
    (values (cons head acc) tail)))
