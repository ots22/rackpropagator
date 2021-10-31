#lang racket/base

(require "common.rkt")

(provide (all-defined-out))

(define plt-tr1
  (make-bib
   #:title    "Reference: Racket"
   #:author   (authors "Matthew Flatt" "PLT")
   #:date     "2010"
   #:location (techrpt-location #:institution "PLT Inc."
                                #:number "PLT-TR-2010-1")
   #:url      "http://racket-lang.org/tr1/"))
 
