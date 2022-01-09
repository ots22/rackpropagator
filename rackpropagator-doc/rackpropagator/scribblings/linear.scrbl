#lang scribble/manual

@require["common.rkt"]

@title{Linear generic interface}

@deftogether[(@defthing[#:kind "interface" gen:linear any/c]
              @defproc[(linear? [v any/c]) boolean?]
              @defthing[#:kind "contract" linear/c contract?])]{

A generic interface representing a type or family of types that can be
given the structure of a vector space (or an inner-product space, if
@racket[dot] is defined). Scalars are represented as any
@racket[number?].

Values of a special type @racket[gen-zero] can be used with this
interface to represent a zero vector of the appropriate type, or the
scalar @racket[0].

The interface contains following methods, that a type @racket[t] can
implement to support the interface:

@itemlist[

@item{@defproc[(linear-add [linear t?] [other (or/c t? gen-zero?)]) t?]{

Addition of two vectors.

See @racket[add].

}}

@item{@defproc[(linear-scale [linear t?] [scalar (or/c number? gen-zero?)]) t?]{

Multiplication of a vector by a scalar.

See @racket[scale].

}}

@item{@defproc[(linear-dot [linear t?] [other (or/c t? gen-zero?)]) number?]{

Inner product of two vectors.

See @racket[dot].

}}



]

These methods are assumed to satisfy the axioms of a vector space (or
inner-product space), but this is not checked.

In addition, the following should hold, for any @racket[v : linear?]:

@itemlist[

@item{@racket[(equal? (linear-add v (gen-zero)) v)]}

@item{@racket[(equal? (linear-scale v (gen-zero)) (linear-scale v 0))]}

@item{@racket[(equal? (linear-dot v (gen-zero)) (gen-zero))]} 

]

}

@defproc[(scale [v any/c] [a (or/c number? gen-zero?)]) any/c]{

If @racket[v] is @racket[linear?] the result is the same as
@racket[linear-scale].

Otherwise, if @racket[a] is @racket[gen-zero], the result is also
@racket[gen-zero], and an error is raised for any other value of
@racket[a].

This function should be used for a recursive method definition of
@racket[linear-scale].

}

@defproc[(add [u any/c] [v any/c]) any/c]{

If @racket[v] is @racket[linear?] the result is the same as
@racket[linear-add].

Otherwise, if @racket[v] is @racket[gen-zero], @racket[u] is returned,
and an error is raised for any other value of @racket[a].

This function should be used for a recursive method definition of
@racket[linear-add].

}

@defproc[(dot [u any/c] [v any/c]) any/c]{

If @racket[v] is @racket[linear?] the result is the same as
@racket[linear-dot].

Otherwise, if @racket[v] is @racket[gen-zero], the result is
@racket[gen-zero], and an error is raised for any other value of
@racket[a].

This function should be used for a recursive method definition of
@racket[linear-dot].

}

@defstruct*[gen-zero ()]{

A type-generic representation of zero.

}

@section{Instances}

Two values @deftech{conform} if any of the following statements are
true:

@itemlist[

@item{They are both numbers}

@item{They are both @racket[null]}

@item{They are both pairs, whose @racket[car] and @racket[cdr] conform}

@item{They are both @racket[vector]s, where each element of one vector
conforms with the corresponding element of the other}

@item{They are both @racket[array]s of compatible shape and with
conforming elements}

@item{They are both @racket[proc-result] structs, with both slots
containing conforming values}

@item{They are both boxes, with conforming contents}

@item{They are both hash tables with the same keys, and where the
values mapped to by each key conform.}

]

Instances of @racket[gen:linear] are defined for @racket[number?],
@racket[null?], @racket[pair?], @racket[vector?], @racket[array?],
@racket[proc-result?], @racket[box?] and @racket[hash?] so that any
equivalence class of conforming values forms an inner-product space,
where addition and scalar multiplication defined elementwise, and an
inner product as the sum of the elementwise product.


@section{Utilities for generic zero}

@defproc[(zero [u linear?]) linear?]{

The zero vector of the same type as @racket[u].

The same as @racket[(scale u (gen-zero))].
}

@defproc[(coerce-zero [u (or/c linear? gen-zero?)] [v linear?]) linear?]{

The same as @racket[(add u (zero v))].

Returns a value like @racket[u], but where any @racket[gen-zero]
occuring as all or part of @racket[u] is replaced with a concrete zero
of the appropriate kind, such that it conforms to @racket[v].

If @racket[u] is not (or does not contain) @racket[gen-zero], the
result is identically @racket[u].  Notice too the effect of
@racket[gen-zero] in the last tail of a dotted list.

@examples[#:eval the-eval

(define z (gen-zero))

(coerce-zero '(1 2 3) '(100 200 300))

(coerce-zero z '(100 200 300))

(coerce-zero `(1 ,z 3) '(100 200 300))

(coerce-zero `(,z ,z 3 4 . ,z) '(100 (200 201) 300 400))

(coerce-zero `(,z ,z 3 4 . ,z) '(100 (200 201) 300 400 . 500))

]


}

@deftogether[(@defproc[(car0 [p (or/c pair? gen-zero?)]) any/c]
              @defproc[(cdr0 [p (or/c pair? gen-zero?)]) any/c]
              @defproc[(null0? [v any/c]) boolean?]
              @defproc[(foldl0 [proc procedure?]
                               [init any/c]
                               [lst* (list*of any/c null0?)])
                       any/c])]{

Like @racket[car], @racket[cdr], @racket[null?] and @racket[foldl]
(the latter for a single list argument only), but generalized to
handle values produced with @racket[gen-zero].

When applying @racket[foldl0], an improper list argument @racket[lst*]
with a value of @racket[gen-zero] in its final position is accepted
and is treated the same as if it were a proper list (that is, as if
it's final tail were @racket[null] instead).

@examples[#:eval the-eval

(car0 '(1 2 3))

(cdr0 (gen-zero))

(foldl0 * 1 (gen-zero))

(foldl0 cons '() (list* 1 2 3 (gen-zero)))

]

}
