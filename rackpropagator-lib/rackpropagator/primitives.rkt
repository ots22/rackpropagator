#lang racket/base

(require racket/list
         racket/function
         racket/vector
         racket/unsafe/ops
         syntax/parse/define

         (prefix-in kern: (only-in '#%kernel apply))
         (prefix-in pre: (only-in racket/private/pre-base new-apply-proc))

         "builtins.rkt"
         "derivative.rkt"
         "prim-definition.rkt"

         (for-syntax "builtins.rkt"
                     racket/base
                     racket/list
                     racket/syntax)

         (for-template racket/base))


;; Helper for defining and providing backpropagator forms
;;
;; f-spec (e.g. (f xs ...)) should refer to an existing binding, and
;; the provided definition should be equivalent to the existing
;; definition. The definition is used to produce a reverse-transformed
;; definition, which is attached to the original binding.  This avoids
;; the need to hand-code a backpropagator, but still permits attaching
;; it to an existing binding (unlike define/D+, which produces a new
;; binding).
;;
;; Each primitive used in the body must already be defined.
(define-syntax-parse-rule (define+provide-backprop-for f-spec:prim-spec
                            body ...)
  #:with f-prim (generate-temporary #'f-spec.prim-id)
  (begin
    (define/D+ (f-prim . f-spec.vars)
      (define f-spec body ...)
      f-spec.appl)
    (register-primitive! f-spec.prim-id (lift/D+ f-prim))
    (provide f-spec.prim-id)))


;; Uses of (racket) apply expand into either a use of apply from
;; '#%kernel, or new-apply-proc from pre-base.  Handle these cases
;; specially (and with the same transform).
(define-syntax-rule (register-apply-proc! proc-id)
  (register-primitive!
   proc-id
   (λ (f . args)
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
                        (list* '() ^f (append head (list tail))))))))))

(register-apply-proc! pre:new-apply-proc)
(register-apply-proc! kern:apply)


(provide + - * sqrt log sin cos asin acos sub1 cons car cdr list
         identity values make-list > < = length null? equal? void)

(require/backprop
 racket
 [(+ . xs)
  (λ (Aw Abox) (cons '() (make-list (length xs) Aw)))]

 [(- . xs)
  (λ (Aw Abox)
    (if (= (length xs) 1)
        (list '() (scale Aw -1))
        (cons '()
              (cons Aw (make-list (sub1 (length xs))
                                  (scale Aw -1))))))]

 [(* x y)
  (λ (Aw Abox) (list '() (scale Aw y) (scale Aw x)))]

 ;; explicit 'if' more compact expansion...
 ;; [(* . xs)
 ;;  (λ (Aw Abox)
 ;;    (cons '()
 ;;          (let loop ([xs xs])
 ;;            (match xs
 ;;              [(list) '()]
 ;;              [(list x) '(Aw)]
 ;;              [(list x y) (list y x)]
 ;;              [(list x ys ..2)
 ;;               (let ([ys* (loop ys)])
 ;;                 (cons (* (car ys) (car ys*))
 ;;                       (map (λ (y) (* x y)) ys*)))]))))]

 ;; [(* . xs)
 ;;  (λ (Aw Abox)
 ;;    (cons '()
 ;;          (let loop ([xs xs])
 ;;            (if (null? xs)
 ;;                '()
 ;;                (let ([xs* (cdr xs)])
 ;;                  (if (null? xs*)
 ;;                      (list Aw)
 ;;                      ;; unroll two argument case
 ;;                      (if (null? (cdr xs*))
 ;;                          (list (scale Aw (car xs*))
 ;;                                (scale Aw (car xs)))
 ;;                          (let ([x (car xs)]
 ;;                                [prods* (loop xs*)])
 ;;                            (cons (scale Aw (* (car xs*) (car prods*)))
 ;;                                  (map (λ (y) (* x y)) prods*))))))))))]

 [(sqrt x)
  (λ (Aw Abox) (list '() (scale Aw (* 0.5 (expt x -0.5)))))]

 [(log x)
  (λ (Aw Abox) (list '() (scale Aw (/ x))))]

 [(sin x)
  (λ (Aw Abox) (list '() (scale Aw (cos x))))]

 [(cos x)
  (λ (Aw Abox) (list '() (scale Aw (- (sin x)))))]

 [(asin x)
  (λ (Aw Abox) (list '() (scale Aw (/ (sqrt (- 1.0 (* x x)))))))]

 [(acos x)
  (λ (Aw Abox) (list '() (scale Aw (- (/ (sqrt (- 1.0 (* x x))))))))]

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

 [(void)
  (λ (Aw Abox) (list '()))]
 ;;
 )


(provide / exp tan atan expt build-list)

(require/primal+backprop
 racket/base

 [/
  (λ xs
    (let ([r (apply / xs)])
      (proc-result
       r
       (λ (Aw Abox)
         (if (null? (cdr xs))
             (list '() (scale Aw (- (* r r))))
             (let* ([x (car xs)]
                    [y (cadr xs)]
                    [inv-y (/ y)])
               (list '()
                     (scale Aw inv-y)
                     (scale Aw (* r inv-y)))))))))]

 [exp
  (λ (x)
    (let ([exp-x (exp x)])
      (proc-result
       exp-x
       (λ (Aw Abox)
         (list '() (scale Aw exp-x))))))]

 [tan
  (λ (x)
    (let ([t (tan x)])
      (proc-result
       t
       (λ (Aw Abox)
         (list '() (scale Aw (/ 1.0 t t)))))))]

 ;; - use case-lambda when supported
 ;; - hypot
 [atan
  (λ (y . x*)
    (let ([x (if (null? x*) 1.0 (car x*))])
      (proc-result
       (atan y x)
       (λ (Aw Abox)
         (let* ([r2 (+ (* x x) (* y y))]
                [Ay (scale Aw (/ x r2))])
           (if (null? x*)
               (list '() Ay)
               (list '() Ay (scale Aw (- (/ y r2))))))))))]

 [expt
  (λ (x y)
    (let ([w (expt x y)])
      (proc-result
       w
       (λ (Aw Abox)
         (list '() (scale Aw (* y (expt x (- y 1)))) (scale Aw (* (log x) w)))))))]

 ;; TODO Aws might be gen-zero
 [build-list
  (λ (n proc)
    (let [(xs* (build-list n proc))]
      (proc-result
       (map primal xs*)
       (λ (Aws Abox)
         (list '() 0
               (foldl0 add (gen-zero)
                       (map (λ (x* Aw) (car ((backprop x*) Aw Abox)))
                            xs* Aws)))))))])


;; result could be (gen-zero)
;; (define-syntax-rule (get-vector-result-sensitivity w Aw Abox)
;;   (if (immutable? w)
;;       Aw
;;       (hash-ref! Abox w (gen-zero))))

;; (define-syntax-rule (get-vector-argument-sensitivity a Abox)
;;   (if (immutable? a)

;;   )


(provide box unbox set-box!)

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
           (list '() Ax))))))])

(require/backprop
 racket/base
 [(unbox b)
  (λ (Aw Abox)
    (let ([Ab (hash-ref! Abox b (box (gen-zero)))])
      (set-box! Ab (add (unbox Ab) Aw))
      (list '() (gen-zero))))]

 [(set-box! b x)
  (λ (Aw Abox)
    (let* ([Ab (hash-ref! Abox b (box (gen-zero)))]
           [Ax (unbox Ab)])
      (set-box! Ab (gen-zero))
      (list '() (gen-zero) Ax)))])


(provide unsafe-car unsafe-cdr)

(require/backprop
 racket/unsafe/ops
 [(unsafe-car xs)
  (λ (Aw Abox) (list '() (cons Aw (gen-zero))))]

 [(unsafe-cdr xs)
  (λ (Aw Abox) (list '() (cons (gen-zero) Aw)))])


(provide add scale dot car0 cdr0 proc-result primal backprop gen-zero
         coerce-zero)

(require/backprop
 "builtins.rkt"
 [(add u v)
  (λ (Aw Abox) (list '() Aw Aw))]

 [(scale v a)
  (λ (Aw Abox) (list '() (scale Aw a) (dot Aw v)))]

 [(dot u v)
  (λ (Aw Abox) (list '() (scale v Aw) (scale u Aw)))]

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

(define+provide-backprop-for (reverse xs)
  (define (rec acc xs)
    (if (null? xs)
        acc
        (rec (cons (car xs) acc) (cdr xs))))
  (rec '() xs))

(define+provide-backprop-for (append . xs)
  (define (rec acc . xs)
    (cond
      [(null? xs) (reverse acc)]
      [(null? (car xs)) (apply rec acc (cdr xs))]
      [else (apply rec (cons (caar xs) acc) (cdar xs) (cdr xs))]))
  (apply rec '() xs))

(define+provide-backprop-for (drop xs n)
  (if (= n 0)
      xs
      (drop (cdr xs) (sub1 n))))

(define+provide-backprop-for (take xs n)
  (if (= n 0)
      '()
      (cons (car xs) (take (cdr xs) (sub1 n)))))


(provide vector-immutable vector vector->list vector->immutable-vector
         list->vector vector-copy make-vector vector-fold)

(require/backprop
 racket/base
 [(vector-immutable . xs)
  (λ (Aw Abox) (list '() (vector->list Aw)))]

 [(vector-length v)
  (λ (Aw Abox) (list '() (gen-zero)))])

(require/primal+backprop
 racket/base
 [vector
  (λ xs
    (let ([v (apply vector xs)])
      (proc-result
       v
       (λ (Aw Abox)
         (let* ([Av (hash-ref! Abox v (gen-zero))]
                [Axs (vector->list (coerce-zero Av v))])
           (hash-set! Abox v (gen-zero))
           (cons '() Axs))))))]

 [vector->list
  ;; v may be mutable or immutable
  (λ (v)
    (let ([w (vector->list v)])
      (proc-result
       w
       (λ (Aw Abox)
         ;; list->vector returns a mutable vector
         (let ([Av (list->vector (coerce-zero Aw w))])
           (if (immutable? v)
               (list '() Av)
               (let ([Av* (hash-ref! Abox v (gen-zero))])
                 ;; could use vector-map! here to mutate the original vector
                 (hash-set! Abox v (add Av Av*))
                 (list '() (gen-zero)))))))))]

 [vector->immutable-vector
  (λ (v)
    (proc-result
     (vector->immutable-vector v)
     (λ (Aw Abox)
       ;; Aw is an immutable vector
       (if (immutable? v)
           (list '() Aw)
           (let* ([Av (hash-ref! Abox v (gen-zero))]
                  [Av* (coerce-zero Av v)])
             (hash-set! Abox v (add Av* Aw))
             (list '() (gen-zero)))))))]

 ;; -> mutable vector
 [list->vector
  (λ (xs)
    (let ([v (list->vector xs)])
      (proc-result
       v
       ;; Aw unused (mutable result)
       (λ (Aw Abox)
         (let* ([Av (hash-ref! Abox v (gen-zero))]
                [Axs (vector->list (coerce-zero Av v))])
           (hash-set! Abox v (gen-zero))
           (list '() Axs))))))]

 ;; v mutable
 [vector-set!
  (λ (v i x)
    (proc-result
     (vector-set! v i x)
     (λ (Aw Abox)
       (let* ([Av (hash-ref! Abox v (make-vector (vector-length v) (gen-zero)))]
              [Ax (vector-ref Av i)])
         (vector-set! Av i (gen-zero))
         (list '() (gen-zero) 0 Ax)))))]

 ;; v mutable or immutable
 [vector-ref
  (λ (v i)
    (proc-result
     (vector-ref v i)
     (λ (Aw Abox)
       (if (immutable? v)
           (let ([Av (build-vector (vector-length v)
                                   (λ (j) (if (= i j)
                                              Aw
                                              (gen-zero))))])
             (list '() Av 0))
           (let ([Av (hash-ref! Abox v
                                (make-vector (vector-length v) (gen-zero)))])
             (vector-set! Av i (add (vector-ref Av i) Aw))
             (list '() (gen-zero) 0))))))])

(require/primal+backprop
 racket/vector
  ;; -> mutable vector
 [vector-copy
  (λ (u)
    (let ([v (vector-copy u)])
      (proc-result
       v
       ;; Aw unused (mutable result)
       (λ (Aw Abox)
         (let ([Av (coerce-zero (hash-ref! Abox v (gen-zero)) v)])
           (hash-set! Abox v (gen-zero))
           (if (immutable? u)
               (list '() (vector->immutable-vector Av))
               (let ([Au* (hash-ref! Abox u (gen-zero))])
                 (hash-set! Abox u (add Av Au*))
                 (list '() (gen-zero)))))))))])

(define/D+ (vector-fold f z v)
  (define n (vector-length v))
  (define (loop acc i)
    (if (< i n)
        (loop (f acc (vector-ref v i)) (+ i 1))
        acc))
  (loop z 0))

(require/primal+backprop
 racket/base
 [make-vector
  (λ (n x)
    (let ([v (make-vector n x)])
      (proc-result
       v
       (λ (Aw-unused Abox)
         (let* ([Av (hash-ref! Abox v (gen-zero))]
                [Av* (coerce-zero Av v)])
           (list '() 0 (vector-fold add (gen-zero) Av*)))))))])

;; TODO explicit transforms for the following (can use these versions
;; to test)

(define+provide-backprop-for (build-vector n proc)
  (define v (make-vector n 0))
  (let loop ([i 0])
    (if (< i n)
        (let ([r (proc i)])
          (vector-set! v i r)
          (loop (+ i 1)))
        v)))

(define (check-vector-lengths len-v0 v0 vs)
  (unless (andmap (λ (v) (equal? len-v0 (vector-length v))) vs)
    (raise-arguments-error 'vector-map
                           "all vectors must have the same size"
                           "vectors:" (cons v0 vs))))

(define+provide-backprop-for (vector-map proc v0 . vs)
  (define len-v0 (vector-length v0))
  (check-vector-lengths len-v0 v0 vs)
  (build-vector len-v0 (λ (i) (apply proc
                                     (vector-ref v0 i)
                                     (map (λ (v) (vector-ref v i)) vs)))))

(define+provide-backprop-for (vector-map! proc v0 . vs)
  (define len-v0 (vector-length v0))
  (check-vector-lengths len-v0 v0 vs)
  (let loop ([i 0])
    (if (< i len-v0)
        (begin
          (vector-set! v0 i
                       (apply proc
                              (vector-ref v0 i)
                              (map (λ (v) (vector-ref v i)) vs)))
          (loop (+ i 1)))
        v0)))


#|

;; TODO: the following is confusing, but I think consistent and correct:

(define vv (vector 1 2 3 4))

;; works as expected:
((grad vector-map! (vector 1 0 0 0)) (lift/D+ +) vv (vector->immutable-vector vv))
;;  ==> '(() #(1 0 0 0) #(1 0 0 0))

;; ((grad vector-map! (vector 1 0 0 0)) (lift/D+ +) vv vv)
'(() #(1 0 0 0) #(0 0 0 0))

;; is an error (vector-map always produces mutable vector output, so perhaps this is unavoidable)
;; ((grad vector-map #(1 0 0 0)) (lift/D+ +) #(1 2 3 4) #(1 2 3 4))

;; works (mutable result)
((grad vector-map (vector 1 0 0 0)) (lift/D+ +) #(1 2 3 4) #(4 5 6 7))

|#

(provide make-empty-hasheq hash-set! hash-ref hash-has-key? hash-ref!)

(require/backprop
 "builtins.rkt"
 [(make-empty-hasheq)
  (λ (Aw Abox) (list '()))])

(require/backprop
 racket/base
 [(hash-set! h k v)
  (λ (Aw Abox)
    (let* ([Ah (hash-ref! Abox h (make-empty-hasheq))]
           [Av (hash-ref! Ah k (gen-zero))])
      (hash-set! Ah k (gen-zero))
      (list '() (gen-zero) (gen-zero) Av)))]

 [(hash-has-key? h k)
  (λ (Aw Abox)
    (list '() (gen-zero) (gen-zero)))]

 [(hash-ref h k)
  (λ (Aw Abox)
    (let* ([Ah (hash-ref! Abox h (make-empty-hasheq))]
           [Ax (hash-ref! Ah k (gen-zero))])
      (hash-set! Ah k (add Ax Aw))
      (list '() (gen-zero) (gen-zero))))])

(define+provide-backprop-for (hash-ref! h k v)
  (unless (hash-has-key? h k)
    (hash-set! h k v))
  (hash-ref h k))


