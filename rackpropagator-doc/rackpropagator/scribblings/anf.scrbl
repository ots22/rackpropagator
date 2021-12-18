#lang scribble/manual

@(require "common.rkt")

@title{A-Normal Form}

@defmodule[rackpropagator/anf]


@~cite[flanagan1993].


@section{A-Normal Form (first kind)}

@racketgrammar*[
#:literals (#%plain-lambda #%plain-app if set! let-values quote quote-syntax)

[anf1-val literal
          id
          (#%plain-lambda formals anf1-expr)]

[anf1-cexpr anf1-val
            (#%plain-app anf1-val anf1-val ...)
            (if anf1-val anf1-expr anf1-expr)
            (set! id anf1-val)]

[anf1-expr anf1-cexpr
           (let-values (((id) anf1-cexpr)) anf1-expr)]

[literal (@#,racket[quote] datum)
         (quote-syntax datum)
         (quote-syntax datum #:local)]

[formals (id ...)
         id
         (id ...+ . id)]

]

@section{A-Normal Form (second kind)}

@racketgrammar*[
#:literals (#%plain-lambda #%plain-app if set! let-values quote quote-syntax)

[anf2-val literal
          id
          (#%plain-lambda formals anf2-expr)]

[anf2-cexpr anf2-val
            (#%plain-app id id ...)
            (if id (#%plain-app id) (#%plain-app id))
            (set! id anf2-val)]

[anf2-expr id
           (let-values (((id) anf2-cexpr)) anf2-expr)]

]

ANF2 has a more restricted grammar than ANF1:
@itemize[
@item{The form in tail position can only be an identifier}
@item{All function applications are made in a @racket[let-values] binding (no explicit tail calls)}
@item{Function application and @racket[if] only use identifiers}
]


@section{Functions for converting to ANF}

@defproc[(anf1-normalize [stx syntax?] [k procedure? identity]) anf1?]{

Convert @racket[stx], which must be fully-expanded syntax, to first A-normal
form.  An explicit continuation @racket[k] may be passed.
}

@defproc[(anf1? [stx syntax?]) boolean?]{
Is @racket[stx] in first A-normal form?
}

@defproc[(anf1->2 [stx anf1?]) anf2?]{
Convert @racket[stx], which must be in first A-normal form, to second A-normal form.
}

@defproc[(anf2-normalize [stx syntax?]) anf2?]{

Convert @racket[stx], which must be fully-expanded syntax, to second A-normal
form.  Equivalent to @racket[(anf1->2 (anf1-normalize stx))].
}

@defproc[(anf2? [stx syntax?]) boolean?]{
Is @racket[stx] in second A-normal form?
}

@defproc[(anf-normalize [stx syntax?]) anf2?]{
The same as @racket[anf2-normalize].
}

@defproc[(anf? [stx syntax?]) boolean?]{
The same as @racket[anf2?].
}

@section{Box mutated bindings}

@defproc[(set!->set-box! [stx anf2?]) anf2?]{

Convert all bindings that are mutated with @racket[set!], to 
@racket[box]ed values, and all uses of @racket[set!] to corresponding
uses of @racket[set-box!].  The resulting syntax is in second A-normal
form, but with no uses of @racket[set!].
}

@section{Utilities}

@defproc[(anf-expand-expression [expr syntax?]) anf2?]{
Fully-expand @racket[expr], convert to ANF2 and convert all uses of
@racket[set!] to @racket[set-box!].  This procedure must be called
during the dynamic extent of a syntax transformer application (see
@racket[syntax-transforming?]).
}

@(require (for-label syntax/id-set syntax/free-vars))
@defproc[(anf-free-vars [stx anf2?]) free-id-set?]{
Returns a set of identifiers that are free within @racket[stx].

Similar to @racket[free-vars] with a true value passed as
@racket[module-bound?], except this procedure does not depend on the
enriched lexical context from expansion.
}

@defproc[(anf-outer-binding [expr anf2?]) syntax?]{
Given an expression in ANF2, extract and return the outermost let-bound
expression.
}

@section{Conventions and syntax classes}

anf-convention

anf-val

anf-binding-expr

anf-expr

anf-simple-literal

