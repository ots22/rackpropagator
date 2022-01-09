#lang scribble/manual

@(require "common.rkt")

@title{Rackpropagator: Reverse-mode automatic differentiation of Racket programs}
@author{Oliver Strickson}
@defmodule[rackpropagator]

Rackpropagator provides an automatic differentiation facility for a
subset of Racket. It uses `reverse mode' differentiation---sometimes
known as @italic{backpropagation}.

The source code of this package is hosted at
@url{https://github.com/ots22/rackpropagator}.

@margin-note{

This library is a work in progress: expect breaking
changes, bugs, and a number of @seclink["Limitations"]{limitations}.

A particular caution: Currently, performance of the generated code can
be @bold{very poor} in some situations.

}

@examples[#:eval the-eval
          #:label "Example:"
  (define/D+ (square x)
    (* x x))

  (square 10.0)

  ((grad square) 10.0)
]

Reverse-mode automatic differentiation is best suited for computing
gradients of functions where the dimension of the domain is large, and
larger than the dimension of the codomain.  Functions to a single
number (from any domain) can have their gradient computed in a single
pass.

@examples[#:eval the-eval
          #:label "Example:"

  (define/D+ (list-norm2 xs)
    (sqrt (apply + (map * xs xs))))

  (list-norm2 '(1.0 1.0))

  (code:comment "∇ is an alias for grad")

  ((∇ list-norm2) (range 10))

  (list-norm2
   (car ((∇ list-norm2) (range 10))))

]

Reverse-mode automatic differentiation is implemented in this library
as a source transformation, at macroexpansion time.  This means that
the transformed code can be compiled and optimized as with any other
function definition, and so---in principle---it can offer similar
performance to a hand-coded derivative.

Differentiable functions can be defined and composed using the
library, and their derivatives taken to any order (again, in
principle, but see @secref{Limitations}).  Derivatives of functions
that close over variables behave as expected, as does mutation.

The system is extensible: While a number of differentiable
`primitives' are pre-defined, it is possible to register new ones.
Reasons for wanting to do this might include supplying derivatives of
functions where it is inconvenient or impossible to define these in
terms of existing primitives (e.g. to support a new array library, or
a function imported via ffi from a numerical library), or for
performance: if a particularly efficient implementation of a
derivative is available, this can be registered and directly used.
Derivatives with respect to numerical arguments can be taken, as well
as vectors, lists, nested and improper lists of any shape, and this
too can be extended to any type that can be given a linear structure.
Support for @secref["array" #:doc '(lib
"math/scribblings/math.scrbl")] is work in progress.

Rackpropagator supports a subset of Racket.  For details, see
@secref{Supported_Language}.

It is possible to differentiate through recursion and many control structures:

@examples[#:eval the-eval
          #:label #f
  (define/D+ (pow x n)
    (if (= n 0)
        1.0
        (* x (pow x (- n 1)))))

  (pow 2.0 3)

  ((grad pow) 2.0 3)
]

Note the type of the result of the following gradient computation,
which is a list containing a single list.  The function takes a single
argument, which is a list, and the gradient returned is with respect
to each element:

@examples[#:eval the-eval
          #:label #f
  (define/D+ (sum-positives lst)
    (for/sum ([elt (in-list lst)]
              #:when (positive? elt))
      elt))

  (define the-list '(1.0 -2.0 3.0 -4.0 5.0))
      
  (sum-positives the-list)

  ((grad sum-positives) the-list)
        ]

Mutation is also supported:

@examples[#:eval the-eval
          #:label #f
(define/D+ (pochhammer3 x0)
  (define next
    (let ([x x0])
      (lambda ()
        (begin0
            x
          (set! x (+ x 1))))))
  (code:comment "only binary multiplication allowed currently")
  (* (next) (* (next) (next))))

((grad pochhammer3) 3.0)
]


If the required derivative information is not available for a binding
then an error occurs---note that @racket[double] in the example below
is defined with racket's @racket[define], rather than
@racket[define/D+].

@examples[#:eval the-eval
          #:label #f
  (define (double x) (+ x x))

  (define/D+ (quadruple x) (double (double x)))

  (eval:error ((grad quadruple) 4.0))
]

By default, the error only occurs if an attempt is made to call the
unknown backpropagator.  This permits code paths that do not
contribute to the derivative (e.g. error handling, tests in
conditionals) without having to register (perhaps meaningless)
derivative information for every function that is called.  This
behaviour can be customised with @racket[current-unknown-transform].




The examples above have all demonstrated finding the gradient of
functions from several arguments to a real number.  Use @racket[D] to
obtain a backpropagator, and supply a sensitivity, to allow other
codomains:

@examples[#:eval the-eval
          #:label #f

(define <-result ((D cons) 3.0 4.0))

(<-result '(1.0 . 0.0))
(<-result '(0.0 . 1.0))

]

If both the primal and gradient are required at the same arguments, use @racket[D+]:

@examples[#:eval the-eval
          #:label #f

(define result ((D+ cons) 3.0 4.0))

(primal result)

(define <-result (backprop result))
(<-result '(1.0 . 0.0))
(<-result '(0.0 . 1.0))

]


@include-section["supported-language.scrbl"]

@include-section["derivative.scrbl"]

@include-section["linear.scrbl"]

@;@include-section["builtins.scrbl"]

@include-section["prim-definition.scrbl"]

@include-section["anf.scrbl"]

@include-section["limitations.scrbl"]


@~cite[pearlmutter2008]

@(generate-bibliography)
