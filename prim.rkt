#lang racket

(require (prefix-in racket: racket/base)
         (prefix-in racket: racket/list)
         (for-syntax syntax/transformer
                     syntax/parse
                     racket/syntax)
         (prefix-in builtin: "primitives.rkt")
         (prefix-in apply: "apply.rkt"))

(provide +
         add
         -
         *
         sub1
         scale
         cons
         car
         cdr
         proc-result
         primal
         backprop
         car0
         cdr0
         cadr
         list
         list*
         identity
         apply
         map
         make-list
         >
         <
         =
         length
         equal?
         gen-zero
         coerce-zero
         make-hasheq
         box
         unbox
         set-box!

         foldl0
         
         define-primitive
         (for-syntax backprop-property-name))

(define-for-syntax backprop-property-name 'backprop);(gensym "backprop"))

(define-syntax (define-primitive stx)
  (syntax-parse stx
    [(define-primitive (f . args)
       {~alt {~once {~seq #:primal f-primal}}
             {~once {~optional {~seq #:box-adjoints box-adjoints-id}}}} ...
       f-backprop-body ...)
     #:with unavail-id (generate-temporary)
     #'(define-syntax f
         (let ([make-backprop (λ (x)
                                (with-syntax ([{~? box-adjoints-id unavail-id} x])
                                  #'(λ args f-backprop-body ...)))])
           ;; No need to use preserved syntax properties here, since
           ;; the syntax-property form appears in the expansion, see
           ;; https://groups.google.com/g/racket-users/c/ZWjpz3kFmjo/m/SUXGVM3YEAAJ
           ;;
           ;; The 'not-free-identifier=? property is added so that the
           ;; binding to f is kept (rather than substituded directly
           ;; with f-primal, dropping the syntax property)
           (make-rename-transformer
            (syntax-property
             (syntax-property
              #'f-primal
              backprop-property-name make-backprop)
             'not-free-identifier=? #t))))]))

(begin-for-syntax
  (define-syntax-class def-formals
    (pattern (f:id x:id ...)
             #:attr app #'(f x ...))
    (pattern (f:id x:id ... . xs:id)
             #:attr app #'(apply f x ... xs))))

(define-syntax (define-simple-primitive stx)
  (syntax-parse stx
    [(define-simple-primitive spec:def-formals
       #:primal f-primal
       f-backprop)
     #'(define-primitive spec
         #:primal f-primal
         (proc-result spec.app f-backprop))]))

(define-simple-primitive (+ . xs)
  #:primal racket:+
  (λ (Aw) (cons '() (make-list (length xs) Aw))))

(define-simple-primitive (add . xs)
  #:primal builtin:add
  (λ (Aw) (cons '() (make-list (length xs) Aw))))

(define-simple-primitive (- . xs)
  #:primal racket:-
  (λ (Aw)
    (cons '()
          (if (= (length xs) 1)
              (scale Aw -1)
              (cons Aw (make-list (sub1 (length xs))
                                  (scale Aw -1)))))))

;; TODO fix signature
(define-simple-primitive (* x y)
  #:primal racket:*
  (λ (Aw) (list '() (scale Aw y) (scale Aw x))))

(define-simple-primitive (sub1 x)
  #:primal racket:sub1
  (λ (Aw) (list '() 1.0)))

(define-simple-primitive (scale v a)
  #:primal builtin:scale
  (λ (Aw) (list '() (scale Aw a) (scale v Aw))))

(define-simple-primitive (cons a b)
  #:primal racket:cons
  (λ (Aw) (list '() (car0 Aw) (cdr0 Aw))))

(define-simple-primitive (car xs)
  #:primal racket:car
  (λ (Aw) (list '() (cons Aw (gen-zero)))))

(define-simple-primitive (cdr xs)
  #:primal racket:cdr
  (λ (Aw) (list '() (cons (gen-zero) Aw))))

(define-simple-primitive (proc-result p b)
  #:primal builtin:proc-result
  (λ (Aw) (list '() (primal Aw) (backprop Aw))))

(define-simple-primitive (primal r)
  #:primal builtin:primal
  (λ (Aw) (list '() (proc-result Aw (gen-zero)))))

(define-simple-primitive (backprop r)
  #:primal builtin:backprop
  (λ (Aw) (list '() (proc-result (gen-zero) Aw))))

(define-simple-primitive (car0 xs)
  #:primal builtin:car0
  (λ (Aw) (list '() (cons Aw (gen-zero)))))

(define-simple-primitive (cdr0 xs)
  #:primal builtin:cdr0
  (λ (Aw) (list '() (cons (gen-zero) Aw))))

(define-simple-primitive (cadr xs)
  #:primal racket:cadr
  (λ (Aw) (list '() (cons (gen-zero (cons Aw (gen-zero)))))))

(define-simple-primitive (list . xs)
  #:primal racket:list
  (λ (Aw) (cons '() Aw)))

;; (define-simple-primitive (list* . xs)
;;   #:primal racket:list*
;;   ;; TODO alternative to split-at
;;   )

(define-simple-primitive (identity x)
  #:primal racket:identity
  (λ (Aw) (list '() Aw)))

(define-primitive (apply f . args)
  #:primal apply:apply
  (let* ([p+b (apply apply f args)]
         [p (primal p+b)]
         [b (backprop p+b)])
    (proc-result p
                 (λ (Aw)
                   (let* ([^f+args (b Aw)]
                          [^f (car ^f+args)]
                          [^args (cdr ^f+args)]
                          [n-1 (sub1 (length args))]
                          [head (take ^args n-1)]
                          [tail (drop ^args n-1)])
                     (list* '() ^f (append head (list tail))))))))

(define (foldl0 f x0 xs)
  (if (or (null? xs) (builtin:gen-zero? xs))
      x0
      (foldl0 f (f x0 (car xs)) (cdr xs))))

(define-primitive (map f . xs)
  #:primal racket:map
  (let* ([p+bs (apply map f xs)]
                [ps (map primal p+bs)]
                [bs (map backprop p+bs)])
           (proc-result
            ps
            (λ (Aws)
              (let* ([^f+xs (map (λ (b Aw) (b Aw)) bs Aws)]
                     [^fs (map car ^f+xs)]
                     ;; list with same length as each element of xs
                     [^xs (map cdr ^f+xs)]
                     ;; 'transpose': list of same length as xs
                     [^xs* (apply map list ^xs)]
                     [^f (foldl0 add (gen-zero) ^fs)])
                (list* '() ^f ^xs*))))))

(define-simple-primitive (make-list n x)
  #:primal racket:make-list
  (λ (Aw) (list '() 0 (foldl0 add (gen-zero) Aw))))

(define-simple-primitive (> . xs)
  #:primal racket:>
  (λ (Aw) (cons '() (make-list (length xs) (gen-zero)))))

(define-simple-primitive (< . xs)
  #:primal racket:<
  (λ (Aw) (cons '() (make-list (length xs) (gen-zero)))))

(define-simple-primitive (= . xs)
  #:primal racket:=
   (λ (Aw) (cons '() (make-list (length xs) (gen-zero)))))

(define-simple-primitive (length lst)
  #:primal racket:length
  (λ (Aw) (list '() (gen-zero))))

(define-simple-primitive (equal? a b)
  #:primal racket:equal?
  (λ (Aw) (cons '() (list (gen-zero) (gen-zero)))))

(define-simple-primitive (gen-zero)
  #:primal builtin:gen-zero
  (λ (Aw) (list '())))

(define-simple-primitive (coerce-zero a b)
  #:primal builtin:coerce-zero
  (λ (Aw) (list '() Aw (gen-zero))))

(define-simple-primitive (make-hasheq)
  #:primal racket:make-hasheq
  (λ (Aw) (list '())))

(define-primitive (box x)
  #:primal racket:box
  #:box-adjoints box-adjoints
  (let* ([b (box x)]
         [__ (hash-set! box-adjoints b (box (gen-zero)))])
    (proc-result
     b
     (λ (Aw-ignore)
       (let* ([Ab (hash-ref box-adjoints b)]
              [Ax (unbox Ab)]
              [__ (set-box! Ab (gen-zero))])
         (list '() Ax))))))

(define-primitive (unbox b)
  #:primal racket:unbox
  #:box-adjoints box-adjoints
  (proc-result
   (unbox b)
   (λ (Aw)
     (let* ([Ab (hash-ref box-adjoints b)]
            [__ (set-box! Ab (add (unbox Ab) Aw))])
       (list '() (gen-zero))))))

(define-primitive (set-box! b x)
  #:primal racket:set-box!
  #:box-adjoints box-adjoints
  (proc-result
   (set-box! b x)
   (λ (Aw-void)
     (let* ([Ab (hash-ref box-adjoints b)]
            [Ax (unbox Ab)]
            [__ (set-box! Ab (gen-zero))])
       (list '() (gen-zero) Ax)))))

(define-for-syntax (prim-definition-old box-adjoints prim)
  (syntax-parse prim
    #:literals (+
                -
                *
                /
                sub1
                add
                scale
                cons
                car
                cdr
                cadr
                unsafe-car
                unsafe-cdr
                car0
                cdr0
                list
                list*
                identity
                apply
                make-list
                gen-zero
                coerce-zero
                proc-result
                primal
                backprop
                <
                >
                =
                length
                equal?
                map
                make-hasheq
                box
                unbox
                unbox0
                set-box!)
    [+
     #'(λ xs
         (proc-result (apply + xs)
                      (λ (Aw) (cons '() (make-list (length xs) Aw)))))]

    [add
     #'(λ xs
         (proc-result (apply add xs)
                      (λ (Aw) (cons '() (make-list (length xs) Aw)))))]

    [-
     #'(λ xs
         (proc-result (apply - xs)
                      (λ (Aw)
                        (cons '()
                              (if (= (length xs) 1)
                                  (scale Aw -1)
                                  (cons Aw (make-list (sub1 (length xs))
                                                      (scale Aw -1))))))))]

    ;; TODO fix signature
    [*
     #'(λ (x y)
         (proc-result (* x y)
                      (λ (Aw) (list '() (scale Aw y) (scale Aw x)))))]

    [sub1
     #'(λ (x)
         (proc-result
          (sub1 x)
          (λ (Aw) (list '() 1.0))))]

    [scale
     #'(λ (v a)
         (proc-result (scale v a)
                      (λ (Aw) (list '() (scale Aw a) (scale v Aw)))))]

    [cons
     #'(λ (a b)
         (proc-result (cons a b)
                      (λ (Aw) (list '() (car0 Aw) (cdr0 Aw)))))]

    [car
     #'(λ (xs)
         (proc-result (car xs)
                      (λ (Aw) (list '() (cons Aw (gen-zero))))))]

    [proc-result
     #'(λ (p b)
         (proc-result (proc-result p b)
                      (λ (Aw) (list '() (primal Aw) (backprop Aw)))))]

    [primal
     #'(λ (r)
         (proc-result (primal r)
                      (λ (Aw) (list '() (proc-result Aw (gen-zero))))))]

    [backprop
     #'(λ (r)
         (proc-result (backprop r)
                      (λ (Aw) (list '() (proc-result (gen-zero) Aw)))))]

    [car0
     #'(λ (xs)
         (proc-result (car0 xs)
                      (λ (Aw) (list '() (cons Aw (gen-zero))))))]

    [cdr
     #'(λ (xs)
         (proc-result (cdr xs)
                      (λ (Aw) (list '() (cons (gen-zero) Aw)))))]

    [cdr0
     #'(λ (xs)
         (proc-result (cdr0 xs)
                      (λ (Aw) (list '() (cons (gen-zero) Aw)))))]

    [cadr
     #'(λ (xs)
         (proc-result (cadr xs)
                      (λ (Aw) (list '() (cons (gen-zero) (cons Aw (gen-zero)))))))]

    [unsafe-car
     #'(λ (xs)
         (proc-result (unsafe-car xs)
                      (λ (Aw) (list '() (cons Aw (gen-zero))))))]
    [unsafe-cdr
     #'(λ (xs)
         (proc-result (unsafe-cdr xs)
                      (λ (Aw) (list '() (cons (gen-zero) Aw)))))]

    [list
     #'(λ xs
         (proc-result xs
                      (λ (Aw) (cons '() Aw))))]

    ;; TODO fix (no multiple values/split-at)
    [list*
     #'(λ xs
         (proc-result
          (apply list* xs)
          (λ (Aw)
            (cons '()
                  (call-with-values
                   (λ () (split-at Aw (sub1 (length xs))))
                   (λ (head tail) (append head (list tail))))))))]

    [identity
     #'(λ (x)
         (proc-result x
                      (λ (Aw) (list '() Aw))))]

    [apply
     #'(λ (f . args)
         (let* ([p+b (apply apply f args)]
                [p (primal p+b)]
                [b (backprop p+b)])
           (proc-result p
                        (λ (Aw)
                          (let* ([^f+args (b Aw)]
                                 [^f (car ^f+args)]
                                 [^args (cdr ^f+args)]
                                 [n-1 (sub1 (length args))]
                                 [head (take ^args n-1)]
                                 [tail (drop ^args n-1)])
                            (list* '() ^f (append head (list tail))))))))]

    [map
     #'(λ (f . xs)
         (let* ([p+bs (apply map f xs)]
                [ps (map primal p+bs)]
                [bs (map backprop p+bs)])
           (proc-result
            ps
            (λ (Aws)
              (let* ([^f+xs (map (λ (b Aw) (b Aw)) bs Aws)]
                     [^fs (map car ^f+xs)]
                     ;; list with same length as each element of xs
                     [^xs (map cdr ^f+xs)]
                     ;; 'transpose': list of same length as xs
                     [^xs* (apply map list ^xs)]
                     [^f (foldl0 add (gen-zero) ^fs)])
                (list* '() ^f ^xs*))))))]

    ;; TODO
    ;; foldl/foldl0

    [make-list
     #'(λ (n x)
         (proc-result (make-list n x)
                      (λ (Aw)
                        (list '() 0 (foldl0 add (gen-zero) Aw)))))]

    [>
     #'(λ xs
         (proc-result
          (apply > xs)
          (λ (Aw)
            (cons '() (make-list (length xs) (gen-zero))))))]

    [<
     #'(λ xs
         (proc-result
          (apply < xs)
          (λ (Aw)
            (cons '() (make-list (length xs) (gen-zero))))))]

    [=
     #'(λ xs
         (proc-result
          (apply = xs)
          (λ (Aw)
            (cons '() (make-list (length xs) (gen-zero))))))]

    [length
     #'(λ (lst)
         (proc-result
          (length lst)
          (λ (Aw)
            (list '() (gen-zero)))))]

    [equal?
     #'(λ (a b)
         (proc-result
          (equal? a b)
          (λ (Aw)
            (cons '() (list (gen-zero) (gen-zero))))))]

    [gen-zero
     #'(λ () (proc-result (gen-zero)
                          (λ (Aw) (list '()))))]

    [coerce-zero
     #'(λ (a b)
         (proc-result
          (coerce-zero a b)
          (λ (Aw) (list '() Aw (gen-zero)))))]

    [make-hasheq
     #'(λ ()
         (proc-result
          (make-hasheq)
          (λ (Aw) (list '()))))]

    [box
     #'(λ (x)
         (let* ([b (box x)]
                [__ (hash-set! box-adjoints b (box (gen-zero)))])
         (proc-result
          b
          (λ (Aw-ignore)
            (let* ([Ab (hash-ref box-adjoints b)]
                   [Ax (unbox Ab)]
                   [__ (set-box! Ab (gen-zero))])
              (list '() Ax))))))]

    [unbox
     #'(λ (b)
         (proc-result
          (unbox b)
          (λ (Aw)
            (let* ([Ab (hash-ref box-adjoints b)]
                   [__ (set-box! Ab (add (unbox Ab) Aw))])
              (list '() (gen-zero))))))]

    [set-box!
     #'(λ (b x)
         (proc-result
          (set-box! b x)
          (λ (Aw-void)
            (let* ([Ab (hash-ref box-adjoints b)]
                   [Ax (unbox Ab)]
                   [__ (set-box! Ab (gen-zero))])
              (list '() (gen-zero) Ax)))))]

    ;; [other #'(if (procedure? other)
    ;;              (unknown-backprop 'other)
    ;;              other)]

    [other
     #'(λ xs
         (proc-result
          (apply (strip-backprop other) xs)
          (λ (Aw)
            (if (gen-zero? Aw)
                Aw
                (unknown-backprop 'other)))))]

    ))
