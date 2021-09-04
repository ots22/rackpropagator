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
         destructuring-sum-let*
         sum-let*)

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


(begin-for-syntax
  (define-syntax-class binding-pair
    (pattern [(x:id) v:expr] ;; single value binding
             #:attr (fresh-mv-binding 1) (list #'[(x) v])
             #:attr id #'x)
    (pattern [(xs:id ...) v:expr]
             ;; need to generate unique-id separately, since xs might be empty
             #:with (tmps ...) (generate-temporaries #'(xs ...))
             #:attr (fresh-mv-binding 1) (syntax->list #'([(tmps ...) v]
                                                           [(xs) tmps] ...))
             #:with unique-id (generate-temporary)
             #:attr id #'unique-id))

  (define-syntax-class binding-list
    (pattern (bs:binding-pair ...)
             #:attr grouped
             (group-by (syntax-parser [b:binding-pair #'b.id])
                       (reverse (syntax->list #'(bs ...)))
                       free-identifier=?)
             )))

;; Like let*-values, but repeated identifiers in the binding forms are
;; accumulated using the generic 'add'.  An identifier can be used in
;; subsequent value expressions, once the definition is complete
;; (after all of a particular identifier have appeared).
(define-syntax (sum-let* stx)
  (syntax-parse stx
    [(_ (b:binding-pair ...) body ...+)
     #:with bs*:binding-list #'(b.fresh-mv-binding ... ...) 
     #:with grouped-pairs (attribute bs*.grouped)
     #:with (ids* ...) (reverse (stx-map stx-caar #'grouped-pairs))
     #:with v*-cmpts (reverse (stx-map (λ (grp) (stx-map stx-cadr grp))
                                       #'grouped-pairs))
     #:with (v* ...) (stx-map (λ (grp)
                                (foldl (λ (a b) #`(add #,a #,b))
                                       (stx-car grp) (zero-cdr (syntax-e grp))))
                              #'v*-cmpts)
     #'(let*-values ([ids* v*] ...) body ...)]))

;; Example:
;;   #'(((a b)) e)
;;      => (list #'((tmp1) e) #'((a) (car tmp1)) #'((tmp2) (cdr tmp1)) #'((b) (car tmp2)))
;;
;; explode-bindings : syntax? -> (listof syntax?)
(define-for-syntax (explode-binding b)
  (syntax-parse b
    [(({}) _) null]
    [((x:id) e) (list b)]
    [(({x . xs}) e)
     #:with tmp (generate-temporary)
     (cons
      #'((tmp) e)
      (append (explode-binding #'((x) (zero-car tmp)))
              (explode-binding #'((xs) (zero-cdr tmp)))))]
    [((xs ...) e)
     #:with (tmps ...) (generate-temporaries #'(xs ...))
     (cons #'((tmps ...) e)
           (apply append
                  (stx-map explode-binding #'(((xs) tmps) ...))))]
    ))

;; Similar to match-let with a nested list pattern, but using sum-let
(define-syntax (destructuring-sum-let* stx)
  (syntax-parse stx
    [(_ (binding ...) body ...+)
     #:with (binding* ...) (apply append (stx-map explode-binding #'(binding ...)))
     #'(sum-let* (binding* ...) body ...)]))

;; ----------------------------------------

(module+ test
  (test-case "non-lazy"
    (check-equal?
     (sum-let* ([(a) 1]
                [(b) 2]
                [(c) 3]
                [(a b) (values 5 6)]
                [(a) b]
                [(c) b]
                [(a) c])
       (list a b c))

     (let* ([a 1]
            [b 2]
            [c 3]
            [a (+ a 5)]
            [b (+ b 6)]
            [a (+ a b)]
            [c (+ c b)]
            [a (+ a c)])
       (list a b c)))

    (check-exn
     exn:fail:syntax?
     (λ ()
       (convert-compile-time-error
        (sum-let* ([(a) 1]
                   ;; can't refer to 'a' here, since its definition is not
                   ;; complete
                   [(b) a]
                   [(a) 2])
          (list a b)))))

    (check-equal?
     (destructuring-sum-let* ([({x x}) '(1 1)]) x)
     2)

    (check-equal?
     (destructuring-sum-let* ([({a b c}) '(1 2 3)]
                              [({d e . e}) '(100 500 . 500)]
                              [({(a b) c ()}) (list (list d d) e)])
       (list a b c))
     '(101 102 1003))

    (check-equal?
     (destructuring-sum-let* ([({a b} c d) (values '(1 2) 3 '(4 5))])
       (list a b c d))
     '(1 2 3 (4 5)))

    (check-equal?
     (destructuring-sum-let* ([({a b} {a b}) (values '(1 2) '(3 4))]
                              [(a b) (values 5 6)]
                              [(b) a])
       (list a b))
     '(9 21))
    
    ))
