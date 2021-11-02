#lang scribble/manual

@(require "common.rkt")

@title{Registering primitives}

@defmodule[rackpropagator/prim-definition]

This module provides the facility for controlling how bindings are
rewritten in order to augment them with a definition of their
backpropagator.

The example below illustrates how to use the high-level interface
(@racket[require/backprop] and @racket[backprop-out] with
@racket[provide]). This should be preferred in user code.

The low-level facility for registering primitives is given by
@racket[local-register-primitive!] and @racket[register-primitive!],
which should be avoided in user code.


@; current-box-adjoints
@; local-register-primitive!
@; register-primitive!
@; prim-definition
@; backprop-out
@; require/backprop


@defform[(local-register-primitive! prim-id prim-augmented-def)]{

Register @racket[prim-id] as a primitive for the current compilation unit.

This function is part of the low-level interface for registering
primitives, and should be avoided by user code.  Consider using
@racket[require/backprop] instead.

@racket[prim-id] is an identifier that binds a function.

@racket[prim-augmented-def] is interpretted as the syntax to use for
the augmented definition of @racket[prim-id] in differentiated code.
It should evaluate to a function that takes the same number of
arguments as @racket[prim-id] and returns a @racket[proc-result]
struct, whose `primal' field (@racket[primal]) contains the result of
evaluting @racket[prim-id] at the arguments given, and whose
`backprop' field (@racket[backprop]) contains a backpropagator function.

The function can share work between the primal and backpropagator.
}

@defform[(register-primitive! prim-id)]{
a.
}

