#lang slideshow

(require slideshow/code)
(require "../../racket-part1/helpers.rkt")

(provide wizards)

(define (wizards)

  (slide
   #:title "Wizards"
   (para #:align 'center
         (bitmap "images/true-names.jpg")
         (bitmap "images/hackers.jpg")
         (bitmap "images/alan-turing.jpg")))

  (slide
   #:title #f
   #:name "Moon"
   (bitmap "images/moon.jpg"))

  (slide
   #:title #f
   #:name "Earthsea"
   (bitmap "images/a-wizard-of-earthsea.jpg"))

  (slide
   #:title "Structure and Interpretation of Computer Programs"

   (para "``It's said that computer science is a lot like magic. And it's sort of good that it's like magic. There's a bad part of computer science that's a lot like religion.''" (blue (t "Abelson (Sussman?), SICP Lecture 2b")))

   #;"Abelson: 38:08 Lecture 2b: Compound Data https://www.youtube.com/watch?v=z1o0WnNch5Y"

   (hc-append 20 (bitmap "images/sicp.jpg") (bitmap "images/compilers.jpg"))
   )

  )
