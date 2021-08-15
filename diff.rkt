#lang racket

(require (for-syntax racket/dict
                     syntax/id-table
                     syntax/parse
                     "reverse-transform.rkt"
                     racket/list
                     racket/dict
                     racket/function
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     syntax/id-table
                     syntax/free-vars
                     syntax/strip-context
                     "anf.rkt"
                     "util.rkt"
                     "closure.rkt"
                     "primitives.rkt")
         "util.rkt"
         "primitives.rkt")

(module+ test
  (require rackunit
           syntax/macro-testing))

(define-for-syntax (prim-definition prim)
  (syntax-parse prim
    #:literals (+)
    [+
     #'(λ xs
         (list (apply + xs)
               (λ (Aw) (cons '() (make-list (length xs) Aw)))))]

    [other #'(if (procedure? other)
                 (λ xs (list (apply other xs)
                             (λ (Aw)
                               (if (zero? Aw)
                                   (cons '() (make-list (length xs) Aw))
                                   (raise-arguments-error 'prim-definition "Backpropagator unknown" "op" 'other))

                                )))
                 other)]))

(define-syntax (D+ stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ e)
     ;;; Currently need to 'expand' here twice. The anf routines work
     ;;; with expanded programs, but also introduce identifiers, which
     ;;; might not be picked up by free-vars unless we expand again.
     #:do [(define simplified-e
             (simple-anf-normalize (local-expand #'e 'expression '())))]
     #:with (_ (((_) e*)) _) (local-expand simplified-e 'expression '())
     #:with (De* (prim:id prim-intro:id) ...) (reverse-transform #'e*)
     #:with (prim-def ...) (stx-map prim-definition #'(prim ...))
     #'(let* ([prim-intro prim-def] ...)
         De*)]))

(module+ test
  ;; This sort of thing fails at the moment
  ;;  (due to the renaming) - can I use the original names somehow? Context wrong at the moment.
  (check-not-exn
   (λ ()
     (convert-compile-time-error
      (let ((y 1))
        (D+ (lambda (x)
              (let ((result (+ x y)))
                result)))))))

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
  
  (test-case "identity"
   (match-let* ([Df (D+ (λ (a)
                          (((λ (b)
                              (λ (c)
                                b)) a) 1)))]
                [v 184.0]
                [(list primal backprop) (Df v)])

     (check-equal? primal 184.0)
     (check-equal? (backprop 1.0) '(() 1.0))))

  (test-case "conditional"
    (match-let* ([Df (D+ (λ (a) (if (> a 10) a 0.0)))]
                 [(list primal1 backprop1) (Df 5.0)]
                 [(list primal2 backprop2) (Df 15.0)])
      (check-equal? primal1 0.0)
      (check-equal? primal2 15.0)
      (check-equal? (backprop1 1.0) '(() 0.0))
      (check-equal? (backprop2 1.0) '(() 1.0))))           
)


;; idea: default expansion to error backpropagator (might not be used!)

;; error messages

;; closures etc

;; more backpropagators

;; think about other lambda formals
