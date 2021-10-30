#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/stx
                     syntax/parse)
         "util.rkt"
         "builtins.rkt")

(provide sum-let*
         destructuring-sum-let*)

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
                                       (stx-car grp) (cdr (syntax-e grp))))
                              #'v*-cmpts)
     #'(let* ([x* v*] ...) body ...)]))

;; Example:
;;   #'((a b) e)
;;      => (list #'(tmp1 e) #'(a (car tmp)) #'(tmp2 (cdr tmp)) #'(b (car tmp2)))
;;
;; explode-bindings : syntax? -> (listof syntax?)
(define-for-syntax (explode-binding b)
  (syntax-parse b
    #:literals (proc-result)
    [(() _) null]

    [((proc-result x y) e)
     #:with tmp (generate-temporary)
     (cons
      #'(tmp e)
      (append (explode-binding #'(x (primal tmp)))
              (explode-binding #'(y (backprop tmp)))))]

    [(x:id e) (list b)]

    [((x . xs) e)
     #:with tmp (generate-temporary)
     (cons
      #'(tmp e)
      (append (explode-binding #'(x (car0 tmp)))
              (explode-binding #'(xs (cdr0 tmp)))))]))

;; Similar to match-let with a nested list pattern, but using sum-let
(define-syntax (destructuring-sum-let* stx)
  (syntax-parse stx
    [(_ (binding ...) body ...+)
     #:with (binding* ...)
            (apply append (stx-map explode-binding #'(binding ...)))
     #'(sum-let* (binding* ...) body ...)]))
