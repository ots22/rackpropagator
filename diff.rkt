#lang racket/base

(require (for-syntax (except-in racket/base apply)
                     racket/list
                     racket/function
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     syntax/id-table
                     syntax/free-vars
                     "apply.rkt"
                     "anf.rkt"
                     "reverse-transform.rkt"
                     "primitives.rkt"
                     "util.rkt")
         racket/list
         racket/function
         racket/unsafe/ops
         "apply.rkt"
         "primitives.rkt")

(provide D+)

(module+ test
  (require racket/match
           syntax/macro-testing
           rackunit))

(define (unknown-backprop op)
  (raise-arguments-error 'prim-definition
                         "Backpropagator unknown"
                         "op" op))

(define-for-syntax (prim-definition prim)
  (syntax-parse prim
    #:literals (+
                -
                *
                /
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
                >
                =
                equal?
                map)
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
                     [^f (foldl add (gen-zero) ^fs)])
                (list* '() ^f ^xs*))))))]

    ;; TODO
    ;; foldl

    [make-list
     #'(λ (n x)
         (proc-result (make-list n x)
                      (λ (Aw) (list '() 0 (apply add Aw)))))]

    [>
     #'(λ xs
         (proc-result
          (apply > xs)
          (λ (Aw)
            (cons '() (make-list (length xs) (gen-zero))))))]

    [=
     #'(λ xs
         (proc-result
          (apply = xs)
          (λ (Aw)
            (cons '() (make-list (length xs) (gen-zero))))))]

    [equal?
     #'(λ (a b)
         (proc-result
          (equal? a b)
          (λ (Aw)
            (cons '() (list (gen-zero) (gen-zero))))))]

    [gen-zero #'(λ () (proc-result (gen-zero)
                                   (λ (Aw) (list '()))))]

    [coerce-zero #'(λ (a b)
                     (proc-result
                      (coerce-zero a b)
                      (λ (Aw) (list '() Aw (gen-zero)))))]

    [other #'(if (procedure? other)
                 (unknown-backprop 'other)
                 other)]
    ))

;; (define-syntax-rule (primal De)
;;   (let-values ([(p b) De]) p))

;; (define-syntax-rule (backprop De)
;;   (let-values ([(p b) De]) b))

;(define primal proc-result-primal)
;(define backprop proc-result-backprop)

(define-syntax (D+ stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ e)
     ;;; Currently need to 'expand' here twice. The anf routines work
     ;;; with expanded programs, but also introduce identifiers, which
     ;;; might not be picked up by free-vars unless we expand again.
     #:do [(define simplified-e
             (anf-normalize (local-expand #'e 'expression '())))]
     #:with (_ (((_) e*)) _) (local-expand simplified-e 'expression '())
     #:with (De* (prim:id prim-intro:id) ...) (reverse-transform #'e*)
     #:with (prim-def ...) (stx-map prim-definition #'(prim ...))
     #'(let* ([prim-intro prim-def] ...)
         (let ([D+f De*])
           (λ xs
             (let ([primal+backprop (apply D+f xs)])
               (proc-result
                (primal primal+backprop)
                (λ Aw
                  (coerce-zero
                   ;; drop terms from closed-over variables
                   (cdr (apply (backprop primal+backprop) Aw))
                   xs)))))))]))

(module+ test
  (test-case "plus"
    (check-not-exn
     (λ ()
       (convert-compile-time-error
        (D+ (λ (x) (+ x x))))))

    (check-not-exn
     (λ ()
       (convert-compile-time-error
        (D+ (λ (x) (let ([f +])
                     (f x x))))))))

  (test-case "closure"
    (check-not-exn
     (λ ()
       (convert-compile-time-error
        (let ((y 1))
          (D+ (lambda (x)
                (let ((result (+ x y)))
                  result))))))))

  (test-case "identity"
    (match-let* ([D+f (D+ (λ (a)
                            (((λ (b)
                                (λ (c)
                                  b)) a) 1)))]
                 [(proc-result* y <-y) (D+f 184.0)])
      (check-equal? y 184.0)
      (check-equal? (<-y 1.0) '(1.0))))

  (test-case "conditional"
    (match-let* ([D+f (D+ (λ (a) (if (> a 10) a 0.0)))]
                 [(proc-result* primal1 backprop1) (D+f 5.0)]
                 [(proc-result* primal2 backprop2) (D+f 15.0)])
      (check-equal? primal1 0.0)
      (check-equal? primal2 15.0)
      (check-equal? (backprop1 1.0) '(0.0))
      (check-equal? (backprop2 1.0) '(1.0))))

  (test-case "pow (Y)"
    (define D+pow
      (D+ (λ (x n)
            (let* ([pow* (λ (x n rec)
                           (if (= n 0)
                               1.0
                               (* x (rec x (- n 1) rec))))]
                   [pow (λ (x n) (pow* x n pow*))])
              (pow x n)))))
    (check-equal? ((backprop (D+pow 2.0 3.0)) 1.0)
                  '(12.0 0.0)))


  ;; TODO letrec
  ;; (test-case "pow"
  ;;   (define D+pow
  ;;     (D+ (λ (x n)
  ;;           (define (pow x n) (if (= n 0) 1.0 (* x (pow x (- n 1)))))
  ;;           (pow x n))))

  ;;   (check-equal? ((backprop (D+pow 2.0 3.0)) 1.0)
  ;;                 '(12.0 0.0)))

  ;; TODO letrec
  ;; (test-case "pow 2"
  ;;   (define D+pow
  ;;     (D+ (λ (x n)
  ;;           (define (pow x n r) (if (= n 0) r (pow x (- n 1) (* r x))))
  ;;           (pow x n 1))))

  ;;   (check-equal? ((backprop (D+pow 2.0 3.0)) 1.0)
  ;;                 '(12.0 0.0)))

  ;; TODO letrec
  ;; (test-case "scale gen-zero"
  ;;   (define D+pow
  ;;     (D+ (λ (x)
  ;;           ;; f accumulates a result into r, but do not use it. Check
  ;;           ;; that the backpropagator can handle scaling by the
  ;;           ;; resulting gen-zero sensitivity.
  ;;           (define (f x r) (if (< x 0) 1 (f (- x 1) (* r x))))
  ;;           (f x 1))))

  ;;   (check-equal? ((backprop (D+pow 2.0)) 1.0)
  ;;                 '(0.0)))

  ;; TODO letrec
  ;; (test-case "Mutual recursion"
  ;;   (define D+fn
  ;;     (D+
  ;;      (λ (x n)
  ;;        (letrec ([f (λ (x n) (if (= n 0)
  ;;                                 x
  ;;                                 (* x (g x (- n 1)))))]
  ;;                 [g (λ (x n) (if (= n 0)
  ;;                                 x
  ;;                                 (+ x (f x (- n 1)))))])
  ;;          (f x n)))))

  ;;   (let-values ([(y <-y) (D+fn 5 5)])
  ;;     (check-equal? y 775)
  ;;     (check-equal? (<-y 1.0) '(585.0 0.0))))

  (test-case "Different final use"
    (check-equal?
     ((backprop ((D+ (λ (x) (let ([b 1]) x))) 2)) 1)
     '(1))

    (let ([D+f (let ([a 1]) (D+ (λ (x) (let ([b a]) x))))])
      (check-equal?
       ((backprop (D+f 2)) 1)
       '(1))))

  (test-case "list/cons/car/cdr"
    (check-equal?
     ((backprop ((D+ (λ (x y) (list (car x) (cdr x) y x y)))
             '(1 2) 3))
      '(1 1 0 (0 . 0) 0))
     '((1 . 1) 0)))

  (test-case "lambda formals"
    (check-equal?
     ((backprop ((D+ (λ xs (car xs))) 2.0 3.0)) 1.0)
     '(1.0 0.0))

    (check-equal?
     ((backprop ((D+ (λ (x . xs) (car xs))) 2.0 3.0 4.0)) 1.0)
     '(0.0 1.0 0.0)))

  (test-case "map"
    (let ([result ((D+ (λ (xs ys) (map * xs ys))) '(1 2 3) '(4 5 6))])
      (check-equal? (primal result) '(4 10 18))
      (check-equal? ((backprop result) '(-1.0 1.0 0.5))
                    '((-4.0 5.0 3.0) (-1.0 2.0 1.5)))))

  (test-case "Primitive binding"
    (define * +)
    (check-exn
     exn:fail?
     (λ () (D+ (λ (x) (* x x))))))

  (test-case "Second derivative"
    (check-equal?
     ((backprop ((D+ (λ (y) ((backprop ((D+ (λ (x) (* x x))) y)) 1.0))) 5.0)) '(1.0))
     '(2.0)))

  ;; This will no longer work: expansion includes a function that has
  ;; an unknown backpropagator
  ;;
  ;; (test-case "match-let"
  ;;   (define Df (D+ (λ (x) (match-let ([(list a b) x]) (+ a b)))))
  ;;   (match-define (list y <-y) (Df '(1 2)))
  ;;   (check-equal? y 3)
  ;;   (check-equal? (<-y 1) '((1 1))))

  ;;
  )

;; TODO

;; error messages (macros/syntax)

;; more backpropagators

;; example with repeated use of D+

;; trick for defining additional primitives nicely/extensibly
;; perhaps both:
;;   - register-backprop
;;   - define/backprop (use reverse-transform/backprop,
;;     then 'register' using the trick)

;; cosmetics for D+:
;;   - explicit closure variables can be passed by user

;; set! (and functions that mutate)
;;   - global table of adjoints (of values - set-box! rather than set!)
;;   - uses of 'box' create something in the adjoint table (representing
;;     the internal state - or something like that)
;;   - convert uses of set! into set-box!
