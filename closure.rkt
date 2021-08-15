#lang racket/base

(provide (struct-out closure))

(struct closure (fn zero)
  #:property prop:procedure (struct-field-index fn)
  #:constructor-name make-closure)

;(define-syntax (closure 
