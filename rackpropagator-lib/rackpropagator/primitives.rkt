#lang racket/base

(require racket/list
         racket/function
         racket/unsafe/ops
         "apply.rkt"
         "builtins.rkt"
         "prim-definition.rkt"
         (for-syntax racket/base
                     syntax/parse))


(provide (backprop-out + - * sub1 cons car cdr cadr list list* identity
                       make-list > < = length equal? make-hasheq unbox set-box!
                       box map unsafe-car unsafe-cdr apply add scale car0 cdr0
                       proc-result primal backprop gen-zero coerce-zero))

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

 [(sub1 x)
  (λ (Aw Abox) (list '() 1.0))]

 [(cons a b)
  (λ (Aw Abox) (list '() (car0 Aw) (cdr0 Aw)))]

 [(car xs)
  (λ (Aw Abox) (list '() (cons Aw (gen-zero))))]

 [(cdr xs)
  (λ (Aw Abox) (list '() (cons (gen-zero) Aw)))]

 [(cadr xs)
  (λ (Aw Abox) (list '() (cons (gen-zero) (cons Aw (gen-zero)))))]

 [(list . xs)
  (λ (Aw Abox) (cons '() Aw))]

 ;; TODO fix (no multiple values/split-at)
 [(list* . xs)
  (λ (Aw Abox)
    (cons '()
          (call-with-values
           (λ () (split-at Aw (sub1 (length xs))))
           (λ (head tail) (append head (list tail))))))]

 [(identity x)
  (λ (Aw Abox) (list '() Aw))]

 ;; TODO
 ;; foldl/foldl0

 [(make-list n x)
  (λ (Aw Abox) (list '() 0 (foldl0 add (gen-zero) Aw)))]

 [(> . xs)
  (λ (Aw Abox) (cons '() (make-list (length xs) (gen-zero))))]

 [(< . xs)
  (λ (Aw Abox) (cons '() (make-list (length xs) (gen-zero))))]

 [(= . xs)
  (λ (Aw Abox) (cons '() (make-list (length xs) (gen-zero))))]

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
  ;; Aw should be (void), and the unused
  (λ (Aw Abox)
    (let* ([Ab (hash-ref! Abox b (box (gen-zero)))]
           [Ax (unbox Ab)])
      (set-box! Ab (gen-zero))
      (list '() (gen-zero) Ax)))])


(require/primal+backprop
 racket/base
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

 [map
  (λ (f . xs)
    (let* ([p+bs (apply map f xs)]
           [ps (map primal p+bs)]
           [bs (map backprop p+bs)])
      (proc-result
       ps
       (λ (Aw Abox)
         (let* ([^f+xs (map (λ (b Aw) (b Aw Abox)) bs Aw)]
                [^fs (map car ^f+xs)]
                ;; list with same length as each element of xs
                [^xs (map cdr ^f+xs)]
                ;; 'transpose': list of same length as xs
                [^xs* (apply map list ^xs)]
                [^f (foldl0 add (gen-zero) ^fs)])
           (list* '() ^f ^xs*))))))])


(require/backprop
 racket/unsafe/ops
 [(unsafe-car xs)
  (λ (Aw Abox) (list '() (cons Aw (gen-zero))))]

 [(unsafe-cdr xs)
  (λ (Aw Abox) (list '() (cons (gen-zero) Aw)))])


(require/primal+backprop
 "apply.rkt"
 [apply
  (λ (f . args)
    (let* ([p+b (apply apply f args)]
           [p (primal p+b)]
           [b (backprop p+b)])
      (proc-result p
                   (λ (Aw Abox)
                     (let* ([^f+args (b Aw)]
                            [^f (car ^f+args)]
                            [^args (cdr ^f+args)]
                            [n-1 (sub1 (length args))]
                            [head (take ^args n-1)]
                            [tail (drop ^args n-1)])
                       (list* '() ^f (append head (list tail))))))))])


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


(define (foldl0 f x0 xs)
  (if (or (null? xs) (gen-zero? xs))
      x0
      (foldl0 f (f x0 (car xs)) (cdr xs))))
