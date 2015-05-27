#lang racket

(require redex)

(define-language bool-lang
 (B true
    false
    (V B B))
 (C (V C B)
    (V B C)
    hole))

(define bool-red
 (reduction-relation
  bool-lang
  (-->
   (in-hole C (V false B))
   (in-hole C false)
   V-false)
  (-->
   (in-hole C (V true B))
   (in-hole C B)
   V-true)))

(traces bool-red
 (term
  (V (V true false)
     (V true true))))
