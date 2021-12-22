#lang scribble/manual

@(require "common.rkt")

@title{Rackpropagator: Reverse-mode automatic differentiation of Racket code}
@author{Oliver Strickson}
@defmodule[rackpropagator]

Rackpropagator provides an automatic differentiation facility
(sometimes known as @italic{backpropagation}), for a subset of Racket.

@margin-note{This library is a work in progress: expect breaking
changes, bugs, and a number of @seclink["Limitations"]{limitations}.}

Reverse-mode automatic differentiation is implemented via source
transformation, at macroexpansion time.  This means that the
transformed code can be compiled and optimized as with any other
function definition, and so---in principle---it can offer similar
performance to a hand-coded derivative.

Differentiable functions can be defined and composed using the
library, and their derivatives taken, in principle to any order (but
see @secref{Limitations}).  Derivatives of functions that close over
variables behave as expected, as does mutation.

The system is @italic{extensible}: While a number of differentiable
`primitives' are pre-defined, it is possible to register new ones.
Reasons for wanting to do this might include supplying derivatives of
functions where it is inconvenient or impossible to define these in
terms of existing primitives (e.g. to support a new array library, or
a function imported via ffi from a numerical library), or for
performance: if a particularly efficient implementation of a
derivative is available, this can be registered and directly used.
Derivatives with respect to numerical arguments can be taken, as well
as vectors, lists, nested and improper lists of any shape, and
@secref["array" #:doc '(lib "math/scribblings/math.scrbl")], and this
too can be extended to any type that can be given a linear structure.

Rackpropagator supports a large subset of Racket.  For details, see
@secref{Supported_Language}.

The example below illustrates the use of @racket[define/D+] for
defining a differentiable function, and @racket[grad] for computing
its gradient.

@examples[#:eval the-eval
          #:label #f
  (define/D+ (square x)
    (* x x))

  (square 10.0)

  ((grad square) 10.0)
]

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
        (let ([x* x])
          (set! x (+ x 1))
          x*))))
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

  (eval:error ((grad double) 3.0))

  (define/D+ (quadruple x) (double (double x)))

  (eval:error ((grad quadruple) 4.0))
]

By default, the error only occurs if an attempt is made to call the
unknown backpropagator.  This permits code paths that do not
contribute to the derivative (e.g. error handling, tests in
conditionals) without having to register (perhaps meaningless)
derivative information for every function that is called.  This
behaviour can be customised with @racket[current-unknown-transform].

@examples[#:eval the-eval
          #:label #f

  (define (handle-error) (error "An error occured"))

  (define/D+ (car-or-id v)
    (if (pair? v)
        (car v)
        v))

  ((grad car-or-id) '(1.0 2.0 3.0))

  ((grad car-or-id) 123.0)
  
  (eval:error
    (parameterize ([current-unknown-transform error-unknown-transform])
      ((grad car-or-id) '(1.0 2.0 3.0))))
]


The examples above have all shown functions from several arguments to
a number.  Use the two-argument form of @racket[grad] to supply a
`sensitivity', allow other codomains.

...

Linear algebra

...
          

@include-section["supported-language.scrbl"]

@include-section["derivative.scrbl"]

@include-section["prim-definition.scrbl"]

@include-section["anf.scrbl"]

@include-section["limitations.scrbl"]


@~cite[pearlmutter2008]
@~cite[elliot2018]
@(generate-bibliography)
