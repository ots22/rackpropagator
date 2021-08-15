#lang racket

(require (for-syntax racket/list
                     racket/syntax
                     syntax/parse
                     syntax/stx)

         syntax/parse
         syntax/stx
         racket/syntax

         rackunit
         syntax/macro-testing)

;;;;
;;;;
;;;;
;;;; NOTE: moved contents of this file to various others - leaving a few notes here for now (also to be moved)
;;;;
;;;;
;;;;


;; ----------------------------------------
;; Utilities


;; ----------------------------------------
;; Closure wrapper type
;;
;; Functions being differentiated are wrapped in this closure structure
;; type, so that the corresponding 'zero' to their closed-over
;; variables can be identified.
;;
;; The closure struct should not be provided, since it cannot be used
;; in an arbitrary way by user code (the AD algorithm can identify a
;; specific pattern of use).




;; ----------------------------------------
;; Syntax classes for ANF



;; 
