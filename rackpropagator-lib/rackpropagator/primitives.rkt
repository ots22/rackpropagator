#lang racket

(require racket/unsafe/ops
         "builtins.rkt"
         "apply.rkt"
         (for-syntax syntax/parse))

(provide (for-syntax prim-definition))

(define (unknown-backprop op)
  (raise-arguments-error 'prim-definition
                         "Backpropagator unknown"
                         "op" op))

(define (strip-backprop p)
  (cond
    [(procedure? p)
     (λ xs (strip-backprop (apply p (map strip-backprop xs))))]
    [(proc-result? p) (primal p)]
    [else p]))

(define (foldl0 f x0 xs)
  (if (or (null? xs) (gen-zero? xs))
      x0
      (foldl0 f (f x0 (car xs)) (cdr xs))))

(define-for-syntax (prim-definition box-adjoints prim)
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
     #:with box-adjoints box-adjoints
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
     #:with box-adjoints box-adjoints
     #'(λ (b)
         (proc-result
          (unbox b)
          (λ (Aw)
            (let* ([Ab (hash-ref box-adjoints b)]
                   [__ (set-box! Ab (add (unbox Ab) Aw))])
              (list '() (gen-zero))))))]

    [set-box!
     #:with box-adjoints box-adjoints
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
