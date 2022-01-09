#lang scribble/manual

@(require scribble-math
          scribble-math/dollar
          "common.rkt")

@title[#:style (with-html5 manual-doc-style)]{Automatic differentiation}

@(local-table-of-contents)

@para{}

This section first give some general background on reverse-mode
automatic differentiation, and then describes the interface provided
by this library.  The implementation is largely based on
@citet[pearlmutter2008].

@section{Background}

@subsection{Derivatives}

This section contains a brief recap of differentiation---before we get
to @emph{automatic} differentiation. It largely follows the
introductory sections of @citet[elliot2018].

Suppose @${A} and @${B} are finite-dimensional vector spaces, and that
we have a function

@$${f : A \to B.}

The derivative of @${f} (if it exists) is a map

@$${df : A \to A \to B}

associating a linear map from @${A} to @${B} with each element of
@${A}.  We say that @${df(x)} is the derivative of @${f} at the point
@${x}.

@margin-note{

You may be used to thinking of a derivative as a number, perhaps
written @${\frac{df}{dx}} or @${f^\prime(x)}, or as a matrix @${J_{ij}
= \frac{\partial f_i}{\partial x_j}}.  This presentation is
equivalent, but will have several advantages for our purposes.

}

@margin-note{

For example if @${f(x) = 5x}, the derivative (as defined above) of
@${f} is the linear map

@$${df(x) = \Delta x \mapsto 5 \Delta x,}

and we understand notation such as @${\frac{df}{dx} = 5} to indicate
the coefficients of this map.

}

Sometimes it is convenient to choose bases for @${A} and @${B}, and
represent this linear map as a matrix (which is then known as the
@emph{Jacobian}), but sometimes another representation is preferable.

We will keep the representation of linear maps as functions.
Eventually, just as we think of a (mathematical) function @${f} being
implemented (or approximated by) a Racket function, its derivative
will be a Racket function too.

It is often necessary, eventually, to turn this function into a
numerical representation.  Evaluating the linear map gives a
@emph{directional derivative} (in terms of Jacobians, this would be a
Jacobian-vector product).  Evaluating it for each element of a basis
of @${A} allows us to reconstruct the whole Jacobian.  Notice that to
reconstruct the Jacobian of @${f} at @${x} we would need to make
@${\dim{A}} evaluations of @${df(x)}, regardless of the dimensionality
of @${B}.

@margin-note{

@bold{Example}

@$${
f : \mathbf{R}^3 \to \mathbf{R}^2 \\
f(x,y,z) = (z + 1, xy)
}

then

@$${
\begin{split}
d&f(x,y,z) =\\
 &(\Delta x, \Delta y, \Delta z) \mapsto (\Delta z, y \Delta x + x \Delta y)
\end{split}
}

The directional derivative, in the direction @${(1,0,0)}, is

@$${
df(x,y,z)(1,0,0) = (0,y)
}

Evaluating the map at the standard basis vectors @${(1,0,0)},
@${(0,1,0)} and @${(0,0,1)} gives the Jacobian matrix:

@$${
\begin{pmatrix}

0 & 0 & 1 \\
y & x & 0

\end{pmatrix}
}

The adjoint map is

@$${
\begin{split}
D&f(x,y,z) =\\
 &(\Delta u, \Delta v) \mapsto (y\Delta v, x\Delta v, \Delta u)
\end{split}
}

where @${\Delta u} and @${\Delta v} are sometimes known as
@deftech{sensitivity variables}.  The function is said to map
@emph{output} or @deftech{result sensitivities} to @deftech{argument
sensitivities}.

Notice that it takes just two evaluations of the adjoint map, at
@${(1,0)} and @${(0,1)}, to obtain the same Jacobian as above, at a
cost of two multiplications per evaluation in each case.

}

A case that is often useful in practice is when @${A} is very high
dimensional, and @${B} is @${\mathbf{R}}. Loss functions in
optimization problems have this form, for example.

Handling this case more efficiently is the motivation for
@emph{reverse-mode} AD, which is based on the following idea.

If we further insist that @${A} and @${B} are both equipped with an
inner product, we can obtain the @emph{adjoint} of a linear map @${L :
A \to B}, which is another linear map @${L^* : B \to A}.  This allows
us to define

@$${Df : A \to B \to A\\
    Df(x) = df(x)^*.
}

If @${df(x)} can be represented by the Jacobian matrix @${J}, then the
matrix representation of @${Df(x)} is the @emph{transpose} of the
Jacobian, @${J^T}.

Particularly when referring to its implementation in code, we call
@${Df(x)} the @emph{backpropagator} of @${f} at @${x}.

Returning to the case we considered above, of @${f : A \to
\mathbf{R}}, it would be possible to reconstruct the Jacobian from a
single evaluation of the linear map @${Df(x) : \mathbf{R} \to A}.

The @deftech{gradient} of @${f} is

@$${\nabla f : A \to A\\
\nabla f(x) = Df(x)(1)
}

assuming the usual inner product on @bold{R}.

@subsection{Composition and the chain rule}

Our goal is to be able to differentiate as many (Racket) functions as
possible.  In some cases, we will be content with explicitly providing
a function @racket[Df] that computes the derivative of another
function @racket[f], and associating them somehow.  It would be
unsatisfactory if we had to do this for every @racket[f], though, so we
seek a way of determining the derivative of a function, from its
definition in terms of other functions.  The ability to do this is the
main selling point of of automatic differentiation.  The primary way
(and in some sense, the only way) that this is achieved is via the
chain rule.

The @deftech{chain rule} allows derivatives of compositions of
functions to be related to compositions of their derivatives.  The
chain rule can be expressed in terms of @${d} or @${D}:

@$${d(g \circ f)(x) = dg(f(x)) \circ df(x)}

@$${D(g \circ f)(x) = Df(x) \circ Dg(f(x)).}

Notice the `reverse' order of composition in the right hand side of
the equation immediately above.

@margin-note*{We will focus on @${D} for the rest of the section, but
similar considerations would apply to @${d}.}  Notice too that for
both rules, we need to know @${f(x)} to express the derivative of the
composition (not merely @${Df}).  There is often some shared work
involved in computing @${Df(x)} and @${f(x)}, but this is not apparent
from the usual chain rule, and an interface based on this would not
let us take advantage of it.

Instead, define

@$${D^+f(x) = (f(x), Df(x))}

and now

@$${D^+(g\circ f)(x) = \big(g(f(x)), Df(x) \circ Dg(f(x))\big).}

Notice that @${D^+(g\circ f)} can now be expressed in terms of
@${D^+g} and @${D^+f}.

@subsection{The reverse transform}

The mapping

@$${f \mapsto D^+f}

as implemented in code is the central operation of reverse-mode AD.

Why @emph{reverse} transform?

Notice the composition rule above: Roughly, whereas data flows
`forwards' through the composition @${g \circ f}, the derivatives of
@${f} and @${g} are composed in the opposite order, and so data flows
`in reverse' through them.

@margin-note*{TODO a picture would help here!} Since the output of the
reverse transform combines the function value and its derivative, data
must in fact flow @emph{both} ways.  The idea is to store each
function evaluation on the way `forward', to be consumed by the
appropriate derivative computation on the way back again.

This description is far from complete.  Handling variable assignment
(and repeated use of a variable) as well as mutable state have been
omitted, as have many technical details needed for a practical
implementation.


@section{Reverse transform API}

The previous section defined the reverse transformation as a mapping
@${f \mapsto D^+ f}.  This section describes how it applies to Racket
code. The macros @racket[D+] and @racket[lift/D+] perform
transformations @emph{similar} to this one; @racket[D] and
@racket[grad] are provided as a convenience. Of these,
@racket[lift/D+] is fundamental.

When differentiating an expression, each procedure that is encountered
is replaced with one that computes both the @deftech{primal}---the
undifferentiated function value, and a @deftech{backpropagator}---a
linear function taking an output sensitivity and returning the
argument sensitivities, called when computing derivative values.  The
process of replacing the function with its primal and backpropagator
is known as @deftech{reverse transformation}.

In the example below, the reverse transformation of @racket[*] is
obtained with @racket[lift/D+].  The primal and backpropagator are
returned in a @racket[proc-result] struct.

@examples[#:eval the-eval
          #:label #f
(define result ((lift/D+ *) 4.0 2.5))
;result
(primal result)
(backprop result)
]

Procedures whose definitions occur within the expression being
differentiated can be transformed automatically by the library.  Any
procedure that is used but not defined within the expression must also
be replaced with its reverse transform.  Such procedures are known as
@deftech{primitives}, and include, for example, arithmetic operations.
They must have backpropagators that are known in advance.

In this library, the backpropagator of a function takes two arguments:
the @tech{result sensitivity}, which should @tech{conform} to the
value returned by the function, and the @tech{box sensitivities},
which will be explained below.  The result of evaluating a
backpropagator is a list containing

@itemlist[

@item{A list of the sensitivities of the closed-over variables in the
function (in an unspecified order)}

@item{The @tech{argument sensitivity} for each argument passed to the
function.}

]

The @deftech{box sensitivity} argument to a backpropagator is the way
sensitivities of mutable data structures are handled.  This is a hash
table (satisfying @racket[(and/c hash? hash-eq? (not immutable?))])
mapping a mutable data structure to its corresponding sensitivity
value.  The value in the hash table with a given mutable data
structure as its key can be updated by the backpropagator of a
function that refers to an element of the data structure.

Continuing the example above,
@examples[#:eval the-eval
          #:label #f

((backprop result) 1.0 (make-hasheq))

]

Notice the empty hash table passed as the second argument, and the
first element of the resulting list, with a list of closed-over
variable sensitivities (in this case there are none, so the list is
empty).

Alternatively, use @racket[D+] to avoid the empty
hash table argument, and to drop the closure sensitivities:

@examples[#:eval the-eval
          #:label #f

(define result ((D+ *) 4.0 2.5))

(primal result)
((backprop result) 1.0)

]

@subsection{Specifying reverse transformations}

When specifying a reverse transform, it should have the form described
above (as returned by @racket[lift/D+]).  Here is the reverse
transformation of two-argument multiplication:

@codeblock{
(λ (x y)
  (proc-result
   (* x y)
   (λ (Aw Abox)
     (list '() (scale Aw y) (scale Aw x)))))
}

and of @racket[exp]:

@codeblock{
(λ (x)
 (let ([exp-x (exp x)])
   (proc-result
     exp-x
     (λ (Aw Abox)
       (list '() (scale Aw exp-x))))))
}

Backpropagator definitions should allow for the fact that the result
sensitivity may be passed a value of @racket[gen-zero] (hence the use
of @racket[scale]).  See @secref{Linear_generic_interface}.

The reverse transform of a binding must be provided when registering a
binding as a new primitive with @racket[register-primitive!], or by
the @racket[require/primal+backprop] mechanism.  It can subsequently
be used in functions defined with @racket[define/D+].

@subsection{Interface}

@defform[(grad expr)]{

A function that evaluates to the gradient of @racket[expr] at the given
arguments.

The result of evaluating @racket[expr] must be a procedure.

This form evaluates to a function of the same arity, that when called
returns the gradient (represented as described above)---in general, it
will be a list whose length is the number of arguments passed, and
whose elements @tech{conform} with the corresponding arguments.

The first form is equivalent to the second with @racket[1.0] passed as
the value of @racket[result-sensitivity].

@examples[#:eval the-eval
((grad (lambda (x y) (+ (* x x) y))) 2.0 3.0)
]

}

@defform[(∇ expr)]{
The same as @racket[grad].
}

@defform[(grad1 expr)]{

Like @racket[grad], but for functions of arity one. When @racket[grad]
would evaluate to a list holding a single element, this form evaluates
to the element without the list wrapper, which may be more convenient.

@examples[#:eval the-eval

((grad1 (grad1 cos)) 0.0)

]

}

@defform[(D expr)]{

Like @racket[D+], except the resulting function returns only the
backpropagator.

@examples[#:eval the-eval
(define/D+ (f x y)
  (vector->immutable-vector
   (vector (* x x) y)))

(((D f) 2.0 3.0) #(1.0 0.0))

(code:comment "An error: sensitivity does not conform with the result:")
(eval:error (((D f) 2.0 3.0) '(1.0 0.0)))
]

}

@margin-note*{TODO Fix unhelpful error message}

@defform[(D+ expr)]{

Like @racket[lift/D+], except that:

@itemlist[

@item{The backpropagator in the result is partially applied in its second
argument to a new empty hash table for holding the sensitivities of
mutable values}

@item{The result of evaluating the backpropagator contains @emph{only}
the argument sensitivities (and not the sensitivies of any closed-over
variables).}

]
}

@defform[(lift/D+ expr)]{

Reverse transform the expression @racket[expr].  The result is a
function, that when called returns a @racket[proc-result] struct
containing the primal at the given arguments, and a backpropagator for
the same arguments.

The backpropagator is the two argument form described
@seclink["Reverse_transform_API"]{above}.  The first argument is the
result sensitivity, and the second must be a mutable hash table
(satisfying @racket[(and/c hash? hash-eq? (not/c immutable?))]).

The resulting function is of the correct form to pass to derivatives
of higher-order functions.

@examples[#:eval the-eval

((grad foldl) (lift/D+ *) 1 '(2 3 4))

]

Using it directly:

@examples[#:eval the-eval
          #:label #f

(define D+f (lift/D+ (lambda (x) (set! x (* x x)) x)))

(define result (D+f 2.0))

(primal result)

((backprop result) 1.0 (make-hasheq))

]

}


@defform*[((define/D+ id expr)
           (define/D+ (id args ... . rest-args)
              body ...+))]{

Define a new primitive in terms of others.

Similarly to define @racket[define], bind @racket[id] to the result of
evaluating @racket[expr] in the first case, or to a procedure in the
second case---note that this form does not support the curried
function definition shorthand of @racket[define].

In addition, the reverse transform of @racket[expr] or @racket[body]
is determined, and @racket[id] registered as a primitive.  Recursive
definitions are allowed.

}

@subsection{Reverse-transformed procedure results}

@deftogether[(@defstruct*[proc-result ([primal any/c]
                                       [backprop procedure?])
                                       #:transparent]
              @defproc[(primal [r proc-result?]) any/c]
              @defproc[(backprop [r proc-result?]) procedure?])]{

A two member structure for holding the return value of a
reverse-transformed procedure.

@racket[proc-result-primal] and @racket[proc-result-backprop] are also
provided under the shorter aliases @racket[primal] and @racket[backprop].

}

@subsection{Handling functions unknown as primitives}

During reverse transformation, an identifier may be encountered that
is not registered as a primitive. In this case, it is transformed to
the result of calling the procedure stored as the value of the
parameter @racket[current-unknown-transform]. In general, the job of
this procedure is to raise an error, but a few other cases where the
result is known may also be handled.

The default value of @racket[current-unknown-transform] is
@racket[error-non-zero-sensitivity-transform], that raises an error
only if an attempt is made to call the unknown backpropagator with a
non-zero sensitivity argument.  This permits code paths that do not
contribute to the derivative (e.g. error handling, tests in
conditionals) without having to register (perhaps meaningless)
derivative information for every function that is called.

@racket[error-unknown-transform] and
@racket[error-unknown-proc-transform] can be very useful for
debugging.

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
