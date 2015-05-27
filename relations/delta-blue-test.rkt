#lang typed/racket

(require "delta-blue-lisp.rkt")

(define x 0)
(define y 1)
(define z 2)
(define (show-variables)
  (printf "x=~s y=~s z=~s\n" x y z))

(define x-var (create-variable))
(define y-var (create-variable))
(define z-var (create-variable))
(define c (create-constraint
           (list x-var y-var z-var)
           medium-strength
           #f
           (list
            ;; x + y = z
            (create-method (lambda () (set! x (- z y))) 0)
            (create-method (lambda () (set! y (- z x))) 1)
            (create-method (lambda () (set! z (+ x y))) 2))))

(show-variables)
(printf "add medium x + y = z\n")
(add-constraint c)
(show-variables)

(define d (create-constraint
           (list x-var)
           weak-strength
           #f
           (list
            ;; x = 10
            (create-method (lambda () (set! x 10)) 0))))

(printf "add weak x = 10\n")
(add-constraint d)
(show-variables)

(define e (create-constraint
           (list y-var)
           strong-strength
           #f
           (list
            ;; y= 20
            (create-method (lambda () (set! y 20)) 0))))

(printf "add strong y = 20\n")
(add-constraint e)
(show-variables)

(define f (create-constraint
           (list x-var y-var)
           strong-strength
           #f
           (list
            ;; x + y = 10
            (create-method (lambda () (set! x (- 10 y))) 0)
            (create-method (lambda () (set! y (- 10 x))) 1))))

(printf "add strong x + y = 20\n")
(add-constraint f)
(show-variables)

(printf "remove strong constraint y=20\n")
(remove-constraint e)
(show-variables)
