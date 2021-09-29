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
         destructuring-sum-let*)

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

(define-for-syntax (group-zeros grp)
  (syntax-parse grp
    #:literals (gen-zero)
    [({~and p [x (gen-zero)]} ps ...)
     #:with ((zs ...) (ps* ...)) (group-zeros #'(ps ...))
     #'((p zs ...) (ps* ...))]

    [(p ps ...)
     #:with ((zs ...) (ps* ...)) (group-zeros #'(ps ...))
     #'((zs ...) (p ps* ...))]
    
    [() #'(() ())]))

(define-for-syntax (filter-zeros grp)
  (syntax-parse (group-zeros grp)
    [((z zs ...) ()) #'(z)]
    [((zs ...) (ps ...+)) #'(ps ...)]))


;; Like let*, but repeated identifiers in the binding forms are
;; accumulated using the generic 'add'.  An identifier can be used in
;; subsequent value expressions, once the definition is complete
;; (after all of a particular identifier have appeared).
(define-syntax (sum-let* stx)
  (syntax-parse stx
    [(_ ([x:id v:expr] ...) body ...+)
     #:with grouped-pairs (group-by stx-car
                                    (reverse (syntax-e #'([x v] ...)))
                                    free-identifier=?)

     ;; optimization: drop constant zero terms
     #:with grouped-pairs* (stx-map filter-zeros #'grouped-pairs)
     #:with (x* ...) (reverse (stx-map stx-caar #'grouped-pairs*))
     #:with v*-cmpts (reverse (stx-map (λ (grp) (stx-map stx-cadr grp))
                                       #'grouped-pairs*))
     #:with (v* ...) (stx-map (λ (grp)
                                (foldl (λ (a b) #`(add #,a #,b))
                                       (stx-car grp) (zero-cdr (syntax-e grp))))
                              #'v*-cmpts)
     #'(let* ([x* v*] ...) body ...)]))

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
(define-syntax (destructuring-sum-let* stx)
  (syntax-parse stx
    [(_ (binding ...) body ...+)
     #:with (binding* ...)
            (apply append (stx-map explode-binding #'(binding ...)))
     #'(sum-let* (binding* ...) body ...)]))

;; ----------------------------------------

(module+ test
  (check-equal?
   (sum-let* ([a 1]
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
   exn:fail?
   (λ ()
     (convert-compile-time-error
      (sum-let* ([a 1]
                 ;; can't refer to 'a' here, since its definition is not
                 ;; complete
                 [b a]
                 [a 2])
        (list a b)))))

  (check-equal?
   (destructuring-sum-let* ([(x x) '(1 1)]) x)
   2)
  
  (check-equal?
   (destructuring-sum-let* ([(a b c) '(1 2 3)]
                            [(d e . e) '(100 500 . 500)]
                            [((a b) c ()) (list (list d d) e)])
     (list a b c))
   '(101 102 1003)))
