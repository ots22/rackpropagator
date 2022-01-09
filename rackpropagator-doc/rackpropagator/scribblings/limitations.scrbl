#lang scribble/manual

@require{common.rkt}

@title{Limitations}

@itemlist[

@item{performance of the generated code can be very poor in some
situations.}

@item{Differentiating involves first @seclink["fully-expanded" #:doc
'(lib "scribblings/reference/reference.scrbl")]{expanding} the given
expression. The expansion might include bindings not provided by any
module. It may be inconvenient or impossible to attach backpropagators
to these.}

@item{Multiple return values are not supported anywhere in the
expansion of the expression being differentiated.}

@item{Use of @racket[case-lambda] is not supported anywhere in the
expansion, which includes any use of keyword arguments or default
values.}

@item{Nested uses of @racket[grad] and @racket[D+] are possible, but
the transformed syntax can get very large from expansion by a constant
factor with each nested use.  The expanded expression is likely to be
subject to extensive optimization by the Racket compiler, but this
doesn't stop the expansion growing very large beforehand.  In
practice, derivatives higher than second order can become unreasonably
large.}

@item{Some error messages could be more informative!}

@item{Continuations, parameters, exceptions and similar are likely to
behave unexpectedly.}

@item{@racket[grad], @racket[D+] (and similar) are syntax, so cannot be
applied.}

]
