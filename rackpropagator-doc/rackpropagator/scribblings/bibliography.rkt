#lang racket/base

(require scriblib/autobib)

(provide (all-defined-out))

(define plt-tr1
  (make-bib
   #:title    "Reference: Racket"
   #:author   (authors "Matthew Flatt" "PLT")
   #:date     "2010"
   #:location (techrpt-location #:institution "PLT Inc."
                                #:number "PLT-TR-2010-1")
   #:url      "http://racket-lang.org/tr1/"))

(define pearlmutter2008
  (make-bib
   #:title    "Reverse-mode AD in a functional framework: Lambda the ultimate backpropagator"
   #:author   (authors "B. A. Pearlmutter" "J. M. Siskind")
   #:date     "2008"
   #:location (journal-location "ACM Trans. Program. Lang. Syst."
                                #:volume 30
                                #:number 2
                                #:pages (list "1" "36"))
   #:url      "https://doi.org/10.1145/1330017.1330018"))

(define elliot2018
  (make-bib
   #:title    "The simple essence of automatic differentiation"
   #:author   "Conal Elliot"
   #:date     "2018"
   #:location (proceedings-location "ACM Program. Lang." #:volume 2 #:series "ICFP")
   #:url      "https://doi.org/10.1145/3236765"))

(define flanagan1993
  (make-bib
   #:title    "The essence of compiling with continuations"
   #:author   (authors "C. Flanagan" "A. Sabry" "B.F. Duba" "M. Felleisen")
   #:date     "1993"
   #:location (proceedings-location "ACM SIGPLAN" #:pages (list "237" "247"))))
