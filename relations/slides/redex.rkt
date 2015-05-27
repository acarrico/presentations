#lang slideshow

(require racket/draw slideshow/repl)
(require "../../racket-part1/helpers.rkt")

(define bool-lang-group (make-repl-group #:prompt "> "))
(define bool-lang-module-backing
  (make-module-backing
   bool-lang-group
   #:module-name "bool-lang.rkt"
   "#lang racket"
   "(require redex)"
   "
(define-language bool-lang
 (B true false (V B B))
 (C (V C B) (V B C) hole))"
   "
(define bool-red
 (reduction-relation
  bool-lang
  (--> (in-hole C (V false B))
       (in-hole C false)
       V-false)
  (--> (in-hole C (V true B))
       (in-hole C B)
       V-true)))"
   "
(traces bool-red
 (term
  (V (V true false)
     (V true true))))"))

(define repl-bg (send the-color-database find-color "Light Blue"))

(provide redex)

(define (redex)
  (slide
   #:title (when-who-what
            "2009"
            "Matthias Felleisen, Robert Findler, Matthew Flatt"
            "Semantics Engineering with PLT Redex")
   #:name "redex"
   (bitmap "images/redex0.jpg")

   (item "lambda, amb, ISWIM, CC, SCC, CK, CEK, SECD, CESK, garbage collection, typed ISWIM"))

  (slide
   #:title "Semantics via Syntax -- eight pages!"
   (bitmap "images/redex-summary.jpg"))

  (slide
   #:title "bool-lang"
   (module-area bool-lang-module-backing #:width (* client-w 5/6) #:height (* client-h 5/8) #:background repl-bg)
   (result-area bool-lang-group #:width (* client-w 5/6) #:height (* client-h 1/6) #:background repl-bg))

  (slide
   #:title "bool-lang traces"
   (bitmap "images/bool-lang.png"))
  )
