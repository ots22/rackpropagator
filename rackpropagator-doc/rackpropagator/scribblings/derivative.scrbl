#lang scribble/manual

@(require "common.rkt")

@title{Automatic differentiation}

@defform*[((grad expr)
           (grad expr result-sensitivity))]{

The first form returns a function .

The second 

Prefer @racket[D+] to multiple uses of @racket[grad] with different
result sensitivities called with the same arguments.

}

@defform*[((∇ expr)
           (∇ expr result-sensitivity))]{
The same as @racket[grad].
}

@defform[(D+ expr)]{ Like @racket[lift/D+], except that:

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

The backpropagator is the two argument form described in ....  The
first argument is the result sensitivity, and the second must be a
mutable hash table (satisfying @racket[(and/c hash? hash-eq? (not/c
immutable?))]).

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
introduce a definition, composing the backpropagators
}

