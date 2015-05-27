#lang slideshow

;; NOTE: arandr can be helpful setting up monitors.
;; NOTE: C-x C-+, C-x C-- to zoom in emacs.
;; NOTE: A-g in slideshow to select a slide.
;; NOTE: avoid bug in old slideshow:
;;       PATH=/home/acarrico/src/git/racket/racket/bin:$PATH
;;
;; C-x C-g l inserts a lambda

(require slideshow/code
         "../racket-part1/helpers.rkt"
         "slides/turtles.rkt"
         "slides/boolean.rkt"
         "slides/wizards.rkt"
         "slides/relations.rkt"
         "slides/redex.rkt"
         "slides/constraints.rkt"
         "slides/cybernetic.rkt"
         #;"slides/repl.rkt")

(define (title-slide)
  (slide
   ;; #:title "Pay Attention to Redex (and Context or Racket Part 4)"
   #:title "Let's Get Cozy with Relations"
   #:layout 'tall
   (t "by Anthony Carrico <acarrico@memebeam.org>")
   (t "https://github.com/acarrico/presentations/")
   (bitmap "images/xkcd-turtles.png")
   (t "@Anthony_Carrico #vtfun @racketlang")
   (t "Vermont Functional Users Group")))

(title-slide)

(slide
 #:title "Context"
 #:layout 'tall
 (blue (item "192x λ-Calculus: Alonzo Church"))
 (subitem (blue (t "1937: Church–Rosser theorem")))
 (subitem "Brian Waters, Lambda Calculus for Devs")
 (blue (item "1934, 1958, 1969: Curry-Howard correspondence"))
 (blue (item "1936: Church–Turing Entscheidungsproblem"))
 (subitem "Eric Smith, The Sep of Church and State")
 (blue (item "1958: Expressions, John McCarthy, Algol60, Lisp"))
 (subitem "Anthony Carrico, Pay Attntn to Racket")
 (blue (item "1964: Peter Landin-ISWIM, Christopher Strachey, Gordon Plotkin, ..."))
 (subitem "Eric Smith, The Next 700 Prog Langs")
 )

(wizards)
(turtles)
(boolean)
(relations)
(redex)
(constraints)
(cybernetic)
(title-slide)
