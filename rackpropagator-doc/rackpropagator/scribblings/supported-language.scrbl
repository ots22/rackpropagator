#lang scribble/manual

@(require "common.rkt")

@title{Supported Language}

@section{Grammar}

Any expression that when @seclink["fully-expanded" #:doc '(lib
"scribblings/reference/reference.scrbl")]{fully expanded} conforms to
@var[differentiable-expr] in the grammar below is amenable to
differentiation (with @racket[grad] and the like).  Evaluating the
resulting derivative will fail unless:

@itemlist[

@item{The expression evaluates to a procedure;}

@item{All of its subexpressions @var[expr] evaluate to a single value; and}

@item{Any procedures called, that are not defined within the
expression, are @seclink["Registering_primitives"]{registered} as
@seclink["Primitives"]{primitives}.}

]

@racketgrammar*[
#:literals (#%expression module module* #%plain-module-begin begin #%provide
            define-values define-syntaxes begin-for-syntax
            #%require #%declare
            #%plain-lambda case-lambda if begin begin0 let-values letrec-values
            set! quote-syntax quote with-continuation-mark
            #%plain-app #%top #%variable-reference)

[differentiable-expr expr
                     (#%expression expr)]

[expr id
      (#%plain-lambda formals expr ...+)
      (if expr expr expr)
      (begin expr ...+)
      (begin0 expr expr ...)
      (let-values ([(id) expr] ...)
        expr ...+)
      (letrec-values ([(id) expr] ...)
        expr ...+)
      (set! id expr)
      (@#,racket[quote] datum)
      (quote-syntax datum)
      (quote-syntax datum #:local)
      (#%plain-app expr ...+)
      (#%variable-reference id)
      (#%variable-reference (#%top . id))
      (#%variable-reference)]

[formals (id ...)
         (id ...+ . id)
         id]]

The grammar is a strict subset of that for fully-expanded expressions.
The differences are: @racket[case-lambda], @racket[#%top] and
@racket[with-continuation-mark] are not included, and
@racket[let-values] and @racket[letrec-values] are restricted to
binding single values.  See also @secref{Limitations}.


@section{Primitives}

@defmodule[rackpropagator/primitives]

This module is required by the @racket[rackpropagator] module, and all
of its exported bindings are re-provided.

The bindings below are registered as primitives when this module is
instantiated.  Registering a backpropagator does not affect the
binding otherwise, so e.g. @racket[+] below will still bind the
regular Racket addition function.

@tabular[(chunk 3 ""
                (map (lambda (export) (to-element (datum->syntax #'ctxt export)))
                       (sort (cons 'apply (map car (cdar exported-primitives)))
                             symbol<?)))
         #:style (style #f
                        (list
                         (attributes '((style . "table-layout: fixed ; width: 100%")))))
         ]
      

Of these, the following are are not documented elsewhere (they are
provided by the @racket[rackpropagator] module):

@defproc[(make-empty-hasheq) (and/c hash? hash-eq? hash-empty?)]{

Return a new mutable hash table exactly like @racket[make-hasheq],
except no initialization argument can be passed (and so the resulting
hash table is always empty).

}

@defproc[(vector-fold [f procedure?] [z any/c] [v vector?]) any/c]{

Like @racket[foldl], but for a single vector argument @racket[v]
(@racket[f] must accept two arguments).

@examples[#:eval the-eval

(vector-fold + 0 #(1 2 3 4 5))

]

}
