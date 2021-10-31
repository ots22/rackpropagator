#lang scribble/manual

@(require "common.rkt")


@title{A-Normal Form}

@racketgrammar*[
#:literals (#%expression #%plain-lambda if begin set!)

[anf1-val simple-literal
          id
          (#%plain-lambda formals M)]
[formals (id ...)
         id
         (id ...+ . id)]
]

@declare-exporting[rackpropagator/anf]

@section{Functions for converting to ANF}

@defproc[(anf1-normalize [stx syntax?] [k procedure? identity]) anf1?]{

Convert @racket[stx] to A-normal form.

}

@defproc[(anf1? [stx syntax?]) boolean?]{
check if stx is in first A-normal form.
}

