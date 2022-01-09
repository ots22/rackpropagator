#lang scribble/manual

@(require "common.rkt")

@title{Registering primitives}

@;; @defmodule[rackpropagator/prim-definition]

This module allows new @tech{primitives} to be registered, by
specifying for each a modified function definition, augmenting the
@tech{primal} computation with a @tech{backpropagator}.

A transformation is associated with a particular binding (and not, for
instance, a procedure value or a symbolic name).

An existing binding can be registered as a primitive with
@racket[register-primitive!], and a binding can be imported into a
module as a newly-registered primitive with @racket[require/backprop]
or @racket[require/primal+backprop].

Once a primitive is registered, the augmented definition is available
from any other module in the namespace where that binding is available
(that is, registering a primitive is an effect visible across module
boundaries).


@defform[(register-primitive! prim-id prim-augmented-def)]{

Registers the identifier @racket[prim-id] as a primitive.

Once registered, the augmented definition is available from any other
module in the same namespace that uses the binding.

@racket[prim-id] is an identifier that binds a procedure.

@racket[prim-augmented-def] is the syntax to use for the augmented
definition of @racket[prim-id] in differentiated code.  It should
evaluate to a function that takes the same number of arguments as
@racket[prim-id] and returns a @racket[proc-result] struct, whose
@racket[primal] field contains the result of evaluting
@racket[prim-id] at the arguments given, and whose @racket[backprop]
field contains a backpropagator function.

The function can share work between the primal and backpropagator.

Associating derivative information with a function confers no overhead
to calls to the @italic{undifferentiated} function.
}


@defform[(require/primal+backprop require-spec [id prim-augmented-def] ...)]{

Imports the names @racket[id ...] from @racket[require-spec] just like
@racket[require], but in addition calls @racket[(register-primitive! id
prim-augmented-def)].

@examples[#:eval the-eval
(require/primal+backprop racket/base
  [exp (lambda (x)
         (let ([exp-x (exp x)])
           (proc-result
             exp-x
             (lambda (Aw Abox)
               (list '() (scale Aw exp-x))))))])
]



}

@defform[(require/backprop require-spec
           [(prim-id args ... . rest-args) backprop-def] ...)]{

Like @racket[require/primal+backprop], but can be used when the primal
is computed as a straightforward application of @racket[prim-id], with
no work shared between the primal and backpropagator. The
backpropagator of @racket[prim-id] is specified as
@racket[backprop-def], and may refer to the arguments @racket[args] or
@racket[rest-args].

@examples[#:eval the-eval
(require/backprop racket/base
  [(sin x) (lambda (Aw Abox)
              (list '() (scale Aw (cos x))))])
]

}
