#lang racket/base

(require racket/list
         racket/function
         racket/unsafe/ops
         ;; "matrix.rkt"
         "builtins.rkt"
         "derivative.rkt"
         "prim-definition.rkt"
         syntax/parse/define
         (for-syntax "builtins.rkt"
                     racket/base
                     racket/list
                     racket/syntax))


;; Helper for defining and providing backpropagator forms
;;
;; f-spec (e.g. (f xs ...)) should refer to an existing binding, and
;; the provided definition should be equivalent to the existing
;; definition. The definition is used to produce a reverse-transformed
;; definition, which is attached to the original binding.  This avoids
;; the need to hand-code a backpropagator, but still permits attaching
;; it to an existing binding (unlike define/D+, which produces a new
;; binding).
(define-syntax-parse-rule (define+provide-backprop-for f-spec:prim-spec
                            body ...)
  #:with f-prim (generate-temporary #'f-spec.prim-id)
  (begin
    (define/D+ (f-prim . f-spec.vars)
      (define f-spec body ...)
      f-spec.appl)
    (register-primitive! f-spec.prim-id (lift/D+ f-prim))
    (provide f-spec.prim-id)))


(provide + - * sub1 cons car cdr list identity values log make-list
         > < = length null? equal? make-hasheq unbox set-box!)

(require/backprop
 racket
 [(+ . xs)
  (λ (Aw Abox) (cons '() (make-list (length xs) Aw)))]

 [(- . xs)
  (λ (Aw Abox)
    (cons '()
          (if (= (length xs) 1)
              (scale Aw -1)
              (cons Aw (make-list (sub1 (length xs))
                                  (scale Aw -1))))))]

 [(* x y)
  (λ (Aw Abox) (list '() (scale Aw y) (scale Aw x)))]

 [(log x)
  (λ (Aw Abox) (list '() (scale Aw (/ 1.0 x))))]

 [(sub1 x)
  (λ (Aw Abox) (list '() 1.0))]

 [(cons a b)
  (λ (Aw Abox) (list '() (car0 Aw) (cdr0 Aw)))]

 [(car xs)
  (λ (Aw Abox) (list '() (cons Aw (gen-zero))))]

 [(cdr xs)
  (λ (Aw Abox) (list '() (cons (gen-zero) Aw)))]

 [(list . xs)
  (λ (Aw Abox) (cons '() Aw))]

 [(identity x)
  (λ (Aw Abox) (list '() Aw))]

 ;; single value only
 [(values x)
  (λ (Aw Abox) (list '() Aw))]

 [(make-list n x)
  (λ (Aw Abox) (list '() 0 (foldl0 add (gen-zero) Aw)))]

 [(> . xs)
  (λ (Aw Abox) (cons '() (make-list (length xs) (gen-zero))))]

 [(< . xs)
  (λ (Aw Abox) (cons '() (make-list (length xs) (gen-zero))))]

 [(= . xs)
  (λ (Aw Abox) (cons '() (make-list (length xs) (gen-zero))))]

 [(null? x)
  (λ (Aw Abox) (cons '() (gen-zero)))]

 [(length lst)
  (λ (Aw Abox) (list '() (gen-zero)))]

 [(equal? a b)
  (λ (Aw Abox) (cons '() (list (gen-zero) (gen-zero))))]

 [(make-hasheq)
  (λ (Aw Abox) (list '()))]

 [(unbox b)
  (λ (Aw Abox)
    (let* ([Ab (hash-ref! Abox b (box (gen-zero)))])
      (set-box! Ab (add (unbox Ab) Aw))
      (list '() (gen-zero))))]

 [(set-box! b x)
  (λ (Aw Abox)
    (let* ([Ab (hash-ref! Abox b (box (gen-zero)))]
           [Ax (unbox Ab)])
      (set-box! Ab (gen-zero))
      (list '() (gen-zero) Ax)))])


(provide vector vector->list list->vector)

(require/backprop
 racket/base
 [(vector . xs)
  (λ (Aw Abox) (list '() (vector->list Aw)))]

 [(vector->list v)
  (λ (Aw Abox) (list '() (list->vector Aw)))]

 [(list->vector lst)
  (λ (Aw Abox) (list '() (vector->list Aw)))])


(provide box exp build-list)

(require/primal+backprop
 racket/base
 [exp
  (λ (x)
    (let ([exp-x (exp x)])
      (proc-result
       exp-x
       (λ (Aw Abox)
         (list '() (scale Aw exp-x))))))]

 [box
  (λ (x)
    (let ([b (box x)])
      (proc-result
       b
       ;; Aw unused
       (λ (Aw Abox)
         (let* ([Ab (hash-ref! Abox b (box (gen-zero)))]
                [Ax (unbox Ab)])
           (set-box! Ab (gen-zero))
           (list '() Ax))))))]

 [build-list
  (λ (n proc)
    (let [(xs* (build-list n proc))]
      (proc-result
       (map primal xs*)
       (λ (Aws Abox)
         (list '() 0
               (foldl0 add (gen-zero)
                       (map (λ (x* Aw) (car ((backprop x*) Aw Abox))) xs* Aws)))))))])


(provide unsafe-car unsafe-cdr)

(require/backprop
 racket/unsafe/ops
 [(unsafe-car xs)
  (λ (Aw Abox) (list '() (cons Aw (gen-zero))))]

 [(unsafe-cdr xs)
  (λ (Aw Abox) (list '() (cons (gen-zero) Aw)))])


(provide add scale car0 cdr0 proc-result primal backprop gen-zero
         coerce-zero)

(require/backprop
 "builtins.rkt"
 [(add . xs)
  (λ (Aw Abox) (cons '() (make-list (length xs) Aw)))]

 [(scale v a)
  (λ (Aw Abox) (list '() (scale Aw a) (scale v Aw)))]

 [(car0 xs)
  (λ (Aw Abox) (list '() (cons Aw (gen-zero))))]

 [(cdr0 xs)
  (λ (Aw Abox) (list '() (cons (gen-zero) Aw)))]

 [(proc-result p b)
  (λ (Aw Abox) (list '() (primal Aw) (backprop Aw)))]

 [(primal r)
  (λ (Aw Abox) (list '() (proc-result Aw (gen-zero))))]

 [(backprop r)
  (λ (Aw Abox) (list '() (proc-result (gen-zero) Aw)))]

 [(gen-zero)
  (λ (Aw Abox) (list '()))]

 [(coerce-zero a b)
  (λ (Aw Abox) (list '() Aw (gen-zero)))])


(define+provide-backprop-for (caar lst) (car (car lst)))

(define+provide-backprop-for (cadr lst) (car (cdr lst)))

(define+provide-backprop-for (cdar lst) (cdr (car lst)))

(define+provide-backprop-for (foldl0 f z xs)
  (if (or (null? xs) (gen-zero? xs))
      z
      (foldl0 f (f z (car xs)) (cdr xs))))

(define+provide-backprop-for (foldl f z xs)
  (if (null? xs)
      z
      (foldl f (f z (car xs)) (cdr xs))))

(define+provide-backprop-for (list* x . xs)
  (if (null? xs)
      x
      (cons x (apply list* xs))))

(define/D+ (map1 f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map1 f (cdr xs)))))

(define+provide-backprop-for (map f xs . xs*)
  (if (null? xs)
      '()
      (cons (apply f (car xs) (map1 car xs*))
            (apply map f (cdr xs) (map1 cdr xs*)))))

(define+provide-backprop-for (append . xs)
  (define (rec acc . xs)
    (cond
      [(null? xs) (reverse acc)]
      [(null? (car xs)) (apply rec acc (cdr xs))]
      [else (apply rec (cons (caar xs) acc) (cdar xs) (cdr xs))]))
  (apply rec '() xs))

(define+provide-backprop-for (reverse xs)
  (define (rec acc xs)
    (if (null? xs)
        acc
        (rec (cons (car xs) acc) (cdr xs))))
  (rec '() xs))

(define+provide-backprop-for (drop xs n)
  (if (= n 0)
      xs
      (drop (cdr xs) (sub1 n))))

(define+provide-backprop-for (take xs n)
  (if (= n 0)
      '()
      (cons (car xs) (take (cdr xs) (sub1 n)))))



;; (provide matrix* matrix-transpose list->matrix matrix->list
;;          matrix-num-rows matrix-num-cols)

;; (require/backprop
;;  "matrix.rkt"
;;  [(matrix* a b)
;;   (λ (Aw Abox)
;;     (list '()
;;           (matrix* Aw (matrix-transpose b))
;;           (matrix* (matrix-transpose a) Aw)))]

;;  [(matrix-transpose a)
;;   (λ (Aw Abox) (list '() (matrix-transpose Aw)))]

;;  [(list->matrix m n a)
;;   (λ (Aw Abox) (list '() 0 0 (matrix->list Aw)))]

;;  [(matrix->list M)
;;   (λ (Aw Abox)
;;     (let ([m (matrix-num-rows M)]
;;           [n (matrix-num-cols M)])
;;       (list '() (list->matrix m n Aw))))]

;;  [(matrix-num-rows M)
;;   (λ (Aw Abox) (list '() (gen-zero)))]

;;  [(matrix-num-cols M)
;;   (λ (Aw Abox) (list '() (gen-zero)))])

;; (provide matrix-inverse)

;; (require/primal+backprop
;;  "matrix.rkt"
;;  [matrix-inverse
;;   (λ (M)
;;     (let ([invM (matrix-inverse M)])
;;       (proc-result
;;        invM
;;        (λ (Aw Abox)
;;          (let ([invMT (matrix-transpose invM)])
;;            (list '() (matrix-scale (matrix* (matrix* invMT Aw) invMT)
;;                                    -1)))))))])




;; Must handle 'apply' specially. This approach means we don't need to
;; introduce an extra binding/wrapper as done previously.
(provide apply)

(begin-for-syntax
  (define apply*-definition
    #'(λ (f . args)
        (let* ([p+b (apply apply f args)]
               [p (primal p+b)]
               [b (backprop p+b)])
          (proc-result p
                       (λ (Aw Abox)
                         (let* ([^f+args (b Aw Abox)]
                                [^f (car ^f+args)]
                                [^args (cdr ^f+args)]
                                [n-1 (sub1 (length args))]
                                [head (take ^args n-1)]
                                [tail (drop ^args n-1)])
                           (list* '() ^f (append head (list tail)))))))))

  (set-prim-definition! (local-expand #'apply 'expression '())
                        apply*-definition)

  (syntax-parse (local-expand #'(apply) 'expression '())
    #:literals (#%plain-app)
    [(#%plain-app apply-proc)
     (set-prim-definition! #'apply-proc apply*-definition)]))
