#lang scribble/manual

@(require "common.rkt")

@title{Builtins}

@;; @defmodule[rackpropagator/builtins]

@(declare-exporting rackpropagator/builtins)

@; All bindings in this module are required and re-provided by the
@; @racket[rackpropagator] module.


@section{Reverse-transformed procedure results}

@deftogether[(@defstruct*[proc-result ([primal any/c]
                                       [backprop procedure?])
                                       #:transparent]
              @defproc[(primal [r proc-result?]) any/c]
              @defproc[(backprop [r proc-result?]) procedure?])]{

A two member structure for holding the return value of a
@tech{reverse-transformed} procedure.

@racket[proc-result-primal] and @racket[proc-result-backprop] are also
provided under the shorter aliases @racket[primal] and @racket[backprop].

}

@section{Handling functions unknown as primitives}

When an identifier is encountered that is not registered as a
primitive, it is transformed according to the
@racket[current-unknown-transform].

The default value of @racket[current-unknown-transform] is
@racket[error-non-zero-sensitivity-transform], that raises an error
only if an attempt is made to call the unknown backpropagator with a
non-zero sensitivity argument.  This permits code paths that do not
contribute to the derivative (e.g. error handling, tests in
conditionals) without having to register (perhaps meaningless)
derivative information for every function that is called.

@defparam[current-unknown-transform proc (-> any/c any/c procedure?)
          #:value error-non-zero-sensitivity-transform]{

The reverse transform to apply to a value not registered as a primitive.

@examples[#:eval the-eval
          #:label "Example:"

  (define/D+ (car-or-void v)
    (when (pair? v)
      (car v)))      

  ((grad car-or-void) '(1.0 2.0 3.0))

  ((grad car-or-void) 123.0)
  
  (eval:error
    (parameterize ([current-unknown-transform error-unknown-transform])
      ((grad car-or-void) '(1.0 2.0 3.0))))
]

}
          
@defproc[(error-unknown-transform [op any/c] [op-name any/c]) any]{
Use as a value for @racket[current-unknown-transform].

The resulting procedure will be used as the reverse transform of
@racket[op].  It will unconditionally raise an error when called.

}

@defproc[(error-unknown-proc-transform [op any/c] [op-name any/c]) procedure?]{
Use as a value for @racket[current-unknown-transform].

The resulting procedure will be used as the reverse transform of
@racket[op].  It will raise an error when @racket[op] is a procedure,
otherwise @racket[op] is returned.

}

@defproc[(error-non-zero-sensitivity-transform [op any/c] [op-name any/c]) procedure?]{
Use as a value for @racket[current-unknown-transform].

The resulting procedure will be used as the reverse transform of
@racket[op].

When @racket[op] is non-procedure value, @racket[op] is returned.

When @racket[op] is a procedure, attempt to construct a reverse
transform for it, whose primal is the result of evaluating the
procedure, and whose backpropagator raises an error when called,
unless @racket[(gen-zero)] is passed (the result of the backpropagator
is then also @racket[(gen-zero)]).

}
