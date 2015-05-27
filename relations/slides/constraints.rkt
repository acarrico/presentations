#lang slideshow

(require slideshow/code)
(require "../../racket-part1/helpers.rkt"
         "delta-blue.rkt")

(provide constraints)

(define (constraints)

  (slide
   #:title "Constraints"

   (when-who-what "1963" "Ivan Sutherland" "Sketchpad, A Man-Machine
Graphical Communication System")
   (blue (para "Ivan Sutherland, ``Armed with the tools for representing and doing arithmetic for constraints I went gaily ahead with programming.''")))

  (slide
   #:title #f
   #:name "sketchpada"
   (scale-to-fit (bitmap "images/sketchpada.jpg") 1000 700))

  (slide
   #:title #f
   #:name "sketchpad0"
   (scale-to-fit (bitmap "images/sketchpad0.jpg") 1000 700))

  (slide
   #:title #f
   #:name "sketchpad1"
   (scale-to-fit (bitmap "images/sketchpad1.jpg") 1000 700))

  (slide
   #:title #f
   #:name "sketchpad2"
   (scale-to-fit (bitmap "images/sketchpad2.jpg") 1000 700))

  (slide
   #:title #f
   #:name "sketchpad3"
   (scale-to-fit (bitmap "images/sketchpad3.jpg") 1000 700))

  (slide
   #:title #f
   #:name "sketchpad4"
   (scale-to-fit (bitmap "images/sketchpad4.jpg") 1000 700))

  (slide
   #:title #f
   #:name "sketchpad5"
   (scale-to-fit (bitmap "images/sketchpad5.jpg") 1000 700))

  (slide
   #:title #f
   #:name "sketchpad6"
   (scale-to-fit (bitmap "images/sketchpad6.jpg") 1000 700))

  (slide
   #:title #f
   #:name "TEXbook"
   (scale-to-fit (bitmap "images/TEXbook.jpg") 1000 700))

  (slide
   #:title "Alan Borning"
   (when-who-what "1979" "Alan Borning" "ThingLab")
   (when-who-what "1993" "Sannella, Maloney, Freeman-Benson, Alan Borning" "Multi-way versus One-way Constraints in User Interfaces: Experience with the DeltaBlue Algorithm")
   (when-who-what "1998" "Greg J. Badros, Alan Borning" "The Cassowary Linear Arithmetic Constraint Solving Algorithm: Interface and Implementation")

   (item "1997: Cassowary in Scwm (Scheme Constraints Window Manager")
   (item "2011: Cassowary in MacOS (Lion)")
   (item "2014: Cassowary in Grid Style Sheets")
  )

  (delta-blue))
