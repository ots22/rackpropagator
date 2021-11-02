#lang scribble/manual

@(require "common.rkt")

@title{Rackpropagator: Automatic differentiation of Racket code}
@author{Oliver Strickson}
@defmodule[rackpropagator]

Rackpropagator is an automatic differentiation library for Racket.

It allows the derivative of a functions defined as a Racket function
to be computed:

@racketblock[
(require rackpropagator)

(D+ (lambda (x) (* x x)))
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
  (define/backprop (f x)
    (* x x))]

The system is @italic{extensible}: While a number of differentiable
`primitives' are pre-defined, it is possible to register new ones.
Reasons for wanting to do this might include supplying derivatives of
functions where it is inconvenient or impossible to define these in
terms of existing primitives (e.g. to support a new array library, or
mathematical function imported via ffi), or for performance reasons,
if a particularly efficient implementation of a particular derivative
is available.

@; @include-section["ad.scrbl"]

@;; perhaps section of the above...
@; @include-section["supported-language.scrbl"]

@include-section["anf.scrbl"]

@include-section["prim-definition.scrbl"]

@; @include-section["trace.scrbl"]

Racket is fun @~cite[plt-tr1].

@(require "bibliography.rkt")

@(generate-bibliography)
