#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     "primitives.rkt")
         syntax/macro-testing
         syntax/parse
         "primitives.rkt")

(provide all-equal?
         pattern-lambda
         pat-λ
         syntax-class->predicate
         destructuring-sum-letrec
         (rename-out [lazy-bind-sum destructuring-sum-lazy-letrec])
         lazy-bind-sum)

(module+ test (require rackunit))

(define (all-equal? . xs)
  (if (null? xs)
      #t
      (andmap (λ (x) (equal? x (car xs))) (cdr xs))))

(define-for-syntax stx-caar (compose1 stx-car stx-car))
(define-for-syntax stx-cadr (compose1 stx-car stx-cdr))

;; like lambda, except all arguments are treated as pattern variables
;; for constructing a syntax object
(define-syntax pattern-lambda
  (syntax-parser
    [(_ (args ...) body)
     #'(lambda (args ...)
         (with-syntax ([args args] ...)
           body))]))

(define-syntax pat-λ (make-rename-transformer #'pattern-lambda))

(define-syntax-rule (syntax-class->predicate stx-class)
  (syntax-parser
    [(~var _ stx-class) #t]
    [_ #f]))

;; Like letrec-values, but repeated identifiers in the binding forms are
;; accumulated using the generic 'add'.  An identifier can be used in
;; subsequent value expressions, once the definition is complete
;; (after all of a particular identifier have appeared).
(define-syntax (sum-letrec stx)
  (syntax-parse stx
    [(_ ([x:id v:expr] ...) body ...+)
     #:with grouped-pairs (group-by stx-car
                                    (reverse (syntax-e #'([x v] ...)))
                                    free-identifier=?)
     #:with (x* ...) (reverse (stx-map stx-caar #'grouped-pairs))
     #:with v*-cmpts (reverse (stx-map (λ (grp) (stx-map stx-cadr grp)) #'grouped-pairs))
     #:with (v* ...) (stx-map (λ (grp)
                                (foldl (λ (a b) #`(add #,a #,b))
                                       (stx-car grp) (zero-cdr (syntax-e grp))))
                              #'v*-cmpts)
     #'(letrec ([x* v*] ...) body ...)]))

;; Example:
;;   #'((a b) e)
;;      => (list #'(tmp1 e) #'(a (car tmp)) #'(tmp2 (cdr tmp)) #'(b (car tmp2)))
;;
;; explode-bindings : syntax? -> (listof syntax?)
(define-for-syntax (explode-binding b)
  (syntax-parse b
    [(() _) null]
    [(x:id e) (list b)]
    [((x . xs) e)
     #:with tmp (generate-temporary)
     (cons
      #'(tmp e)
      (append (explode-binding #'(x (zero-car tmp)))
              (explode-binding #'(xs (zero-cdr tmp)))))]))

;; Similar to match-let with a nested list pattern, but using sum-let
(define-syntax (destructuring-sum-letrec stx)
  (syntax-parse stx
    [(_ (binding ...) body ...+)
     #:with (binding* ...) (apply append (stx-map explode-binding #'(binding ...)))
     #'(sum-letrec (binding* ...) body ...)]))


;; ----------------------------------------
;; lazy

(;; begin-for-syntax
 ;;  (define-syntax-class binding-pair
 ;;    (pattern ((x:id) v:expr) ;; single value binding
 ;;             #:attr id #'x)
 ;;    (pattern ((xs:id ...) v:expr) ;; multiple value binding
 ;;             #:with unique-id (generate-temporary)
 ;;             #:attr id #'unique-id))

 ;;  (define-syntax-class binding-list
 ;;    (pattern (bs:binding-pair ...)
 ;;             #:attr grouped
 ;;             (group-by (syntax-parser [b:binding-pair #'b.id])
 ;;                       (reverse (syntax->list #'(bs ...)))
 ;;                       free-identifier=?)
 ;;             )))

;; (module+ test
;;   (begin-for-syntax
;;     (define example-binding-list
;;       #'([(a) 1] [(c a) (values 2 3)] [(b) 4] [(a) 5] [(a d) (values 6 7)]))

;;     (displayln
;;      (syntax->datum
;;       (syntax-parse example-binding-list
;;         [bs:binding-list (attribute bs.grouped)])))))



;; (define-syntax (sum-lazy-letrec stx)
;;   (syntax-parse stx
;;     [(_ bindings:binding-list body ...+)
;;      #:with grouped-pairs (attribute bindings.grouped)
;;      #:with (x* ...) (reverse (stx-map stx-caar #'grouped-pairs))
;;      #:with v*-cmpts (reverse (stx-map (λ (grp) (stx-map stx-cadr grp)) #'grouped-pairs))
;;      #:with (v* ...) (stx-map (λ (grp)
;;                                 (foldl (λ (a b) #`(add #,a #,b))
;;                                        (stx-car grp) (zero-cdr (syntax-e grp))))
;;                               #'v*-cmpts)
;;      #'(letrec-values ([x* (λ () v*)] ...) body ...)]))

(define-syntax (sum-lazy-letrec stx)
  (syntax-parse stx
    [(_ ([x:id v:expr] ...) body ...+)
     #:with grouped-pairs (group-by stx-car
                                    (reverse (syntax-e #'([x v] ...)))
                                    free-identifier=?)
     #:with (x* ...) (reverse (stx-map stx-caar #'grouped-pairs))
     #:with v*-cmpts (reverse (stx-map (λ (grp) (stx-map stx-cadr grp)) #'grouped-pairs))
     #:with (v* ...) (stx-map (λ (grp)
                                (foldl (λ (a b) #`(add #,a #,b))
                                       (stx-car grp) (zero-cdr (syntax-e grp))))
                              #'v*-cmpts)
     #'(letrec ([x* (λ () v*)] ...) body ...)]))


;; Example:
;;   #'((a b) e)
;;      => (list #'(tmp1 e) #'(a (car tmp)) #'(tmp2 (cdr tmp)) #'(b (car tmp2)))
;;
;; explode-binding : syntax? -> (listof syntax?)
(define-for-syntax (explode-binding-lazily b)
  (syntax-parse b
    [(() _) null]
    [(x:id e) (list b)]
    [((x . xs) e)
     #:with tmp (generate-temporary)
     (cons
      #'(tmp e)
      (append (explode-binding-lazily #'(x (zero-car (tmp))))
              (explode-binding-lazily #'(xs (zero-cdr (tmp))))))]))

;; ;; Similar to match-let with a nested list pattern, but using sum-let
;; (define-syntax (lazy-bind-sum stx)
;;   (syntax-parse stx
;;     [(_ (binding ...) body ...+)
;;      #:with (binding* ...) (apply append (stx-map explode-binding-lazily #'(binding ...)))
;;      #'(sum-lazy-letrec (binding* ...) body ...)]))



(define-syntax (lazy-bind-sum stx) (error 'undefined))


;; ----------------------------------------

(module+ test
  ;; (check-equal?
  ;;  (lazy-bind-sum ([{(x x)} '(1 1)]) (x))
  ;;  2)

  ;; (check-equal?
  ;;  (lazy-bind-sum ([{(a b c)} '(1 2 3)]
  ;;                  [{(d e . e)} '(100 500 . 500)]
  ;;                  [{((a b) c ())} (list (list (d) (d)) (e))])
  ;;                 (list (a) (b) (c)))
  ;;  '(101 102 1003))

  ;; (check-equal?
  ;;  (lazy-bind-sum ([{x} 1]) (x))
  ;;  1)

  ;; (check-equal?
  ;;  (lazy-bind-sum ([{x y} (values 1 2)])
  ;;    (+ (x) (y)))
  ;;  3)

  ;; (check-equal?
  ;;  (lazy-bind-sum ([{x (y z)} (values 1 (list 2 3))])
  ;;    (+ (x) (* (y) (z))))
  ;;  7)

  (test-case "lazy"
    (check-equal?
     (sum-lazy-letrec ([{a} 1]
                       [{b} 2]
                       [{c} 3]
                       [{tmp1 tmp2} (values 5 6)]
                       [{a} (b)]
                       [{c} (b)]
                       [{a} (c)])
                      (list (a) (b) (c) (tmp1) (tmp2)))

     (let*-values ([{a} 1]
                   [{b} 2]
                   [{c} 3]
                   [{tmp1 tmp2} (values 5 6)]
                   [{a} (+ a b)]
                   [{c} (+ c b)]
                   [{a} (+ a c)])
       (list a b c tmp1 tmp2)))

    ;; (check-equal?
    ;;  (sum-lazy-letrec ([a 1]
    ;;                    [b (a)]
    ;;                    [a 2])
    ;;                   (list (a) (b)))
    ;;  '(3 3))




    ;;
    )

  (test-case "non-lazy"
    (check-equal?
     (sum-letrec ([a 1]
                  [b 2]
                  [c 3]
                  [a b]
                  [c b]
                  [a c])
                 (list a b c))

     (let* ([a 1]
            [b 2]
            [c 3]
            [a (+ a b)]
            [c (+ c b)]
            [a (+ a c)])
       (list a b c)))

    (check-exn
     exn:fail:contract:variable?
     (λ ()
       (convert-compile-time-error
        (sum-letrec ([a 1]
                     ;; can't refer to 'a' here, since its definition is not
                     ;; complete
                     [b a]
                     [a 2])
                    (list a b)))))

    (check-equal?
     (destructuring-sum-letrec ([(x x) '(1 1)]) x)
     2)

    (check-equal?
     (destructuring-sum-letrec ([(a b c) '(1 2 3)]
                                [(d e . e) '(100 500 . 500)]
                                [((a b) c ()) (list (list d d) e)])
                               (list a b c))
     '(101 102 1003))
    ;;
    ))
