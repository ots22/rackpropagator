#lang scribble/manual

@(require "common.rkt")

@title{Rackpropagator: Automatic differentiation of Racket code}
@author{Oliver Strickson}
@defmodule[rackpropagator]

@margin-note{WIP}

Rackpropagator is an automatic differentiation library for Racket.

It allows the derivative of a functions defined as a Racket function
to be computed:

@racketblock[
(require rackpropagator)

(D+ (λ (x) (* x x)))
]

Rackpropagator provides reverse mode automatic differentiation
(sometimes known as @italic{backpropagation}) through a transformation
of a function's source-code, at macroexpansion time---this means that
the transformed code can be compiled and optimized.  There is no
run-time machinery associated with computing the derivative (such as
runtime `traces' or `tapes').  Associating a derivative with a
function confers no overhead to calling the undifferentiated function.
See (link) performance.

Rackpropagator supports a subset of Racket.  For details, see (link)
supported language. 

Differentiable functions can be defined and composed using the library
(example), and their derivatives taken to (in principle) any order.
Derivatives of functions that close over variables behave as expected,
as does mutation.

@racketblock[
  (define/D+ (f x)
    (* x x))]

@examples[#:eval the-eval
  (define/D+ (f x)
    (* x x))

  (f 10.0)

  ((grad f) 10.0)]

Alternatively, @racket[grad] has an alias, @racket[∇]:

@examples[#:eval the-eval
 ((∇ f) 10.0)]

@examples[#:eval the-eval
  (define/D+ (pow x n)
    (if (= n 0)
        1.0
        (* x (pow x (- n 1)))))

  ((∇ pow) 2.0 3)

  ((∇ (∇ pow) '(1.0 0.0)) 2.0 3)
]


  


The system is @italic{extensible}: While a number of differentiable
`primitives' are pre-defined, it is possible to register new ones.
Reasons for wanting to do this might include supplying derivatives of
functions where it is inconvenient or impossible to define these in
terms of existing primitives (e.g. to support a new array library, or
mathematical function imported via ffi), or for performance reasons,
if a particularly efficient implementation of a particular derivative
is available.

@section{Limitations}

@itemlist[
@item{procedures only (impersonators etc create difficulties)}
@item{syntax can sometimes expand to unknown/unprovided procedures--no way to attach the required information in this case}
@item{multiple return values are not supported anywhere (which may limit the use of certain constructs, or syntax that transforms to them)}
@item{grad, D+ etc are syntax and cannot be applied}
@item{nested grad is possible, but currently can generates a lot of code (constant factor each time).  This can usually be optimized extensively, but this relies on Racket, and happens *after* the syntax is transformed}
]

@; @include-section["ad.scrbl"]

@;; perhaps section of the above...
@; @include-section["supported-language.scrbl"]

@include-section["anf.scrbl"]

@include-section["prim-definition.scrbl"]

@; @include-section["trace.scrbl"]

Racket is fun @~cite[plt-tr1].

@~cite[elliot2018]

@~cite[pearlmutter2008]

@(generate-bibliography)
