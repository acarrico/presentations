#lang slideshow

(require racket/draw slideshow/repl)

(provide delta-blue)

#;(require "../delta-blue-lisp.rkt")

(define bd-group (make-repl-group #:prompt "> "))

#;(slide
 #:title "repl"
 (repl-area #:height (* client-h 3/4) #:background repl-bg)
 )

(define delta-blue-module-backing
  (make-module-backing
   bd-group #:module-name "delta-blue-test.rkt"
   (port->string (open-input-file "delta-blue-test.rkt"))))

(define repl-bg (send the-color-database find-color "Light Blue"))

(define (delta-blue)
  (slide
   #:title "delta blue"
   (module-area delta-blue-module-backing
                #:width (* client-w 5/6)
                #:height (* client-h 4/9)
                #:background repl-bg)
   (result-area bd-group
                #:width (* client-w 5/6)
                #:height (* client-h 4/9)
                #:background repl-bg)))
