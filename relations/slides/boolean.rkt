#lang slideshow

(require slideshow/code)
(require "../../racket-part1/helpers.rkt")

(provide boolean)
(define (boolean)
  (slide
   #:title "George Boole (1815-1864) — Boolean Algebra"
   (para #:align 'center (bitmap "images/George_Boole_color.jpg")))

  (slide
   #:title (when-who-what "1982" "Forrest Mims III" "Engineer's Notebook II")
   #:name "Engineer's Notebook"
   (para #:align 'center (bitmap "images/Forrest Mims.jpg")))

  (slide
   #:name "Ones and Zeros"
   (para #:align 'center (bitmap "images/ones-and-zeros.jpg")))

  (slide
   #:name "Boolean Reasoning"
   (para "``The operational (i.e. functional) basis of Boolean reasoning differentiates it from the predicate calculus, whose basis is relational''")
   (para #:align 'center (bitmap "images/boolean-reasoning.jpg")))

  (slide
   #:title "Video Game Example"

   (when-who-what "1998" "John Gregg" "Ones and Zeros")

   (item "No minions can teleport while wearing armor.")
   (item "Any minion worth 150 points but still vulnerable to the electro-neural disrupter can teleport.")
   (item "All 150-point minions are armored.")
   (item "Any minon that is electro-neurally disruptable and not worth 150 points is either armored or a teleporter or both.")
   (item "Any minion that is in vulnerable to both of your weapons either can teleport or is not worth 150 points or both")

   (para "Good luck! The fate of humanity rests in your hands!"))

  (slide
   #:title "Blake Canonical Form"

   (when-who-what "1937" "Archie Blake" "Canonical expressions in Boolean algebra.")

   (item "A minion cannot wear armor and teleport")
   (item "All electro-neurally disruptable minions are either armored or can teleport.")
   (item "No minions are worth 150 points"))

  (slide
   #:title #f
   #:name "drudge"

   (para (blue (t "Alan Turing")) ", ``The process of constructing instruction tables should be very fascinating. There need be no real danger of it ever becoming a drudge, for any processes that are quite mechanical may be turned over to the machine itself.''"))

    (slide
   #:title "Subtext 2"

   (when-who-what "OOPSLA'07" "Jonathan Edwards" "No Ifs, Ands, or Buts Uncovering the Simplicity of Conditionals")

   (para (blue (t "Johnathan Edwards")) ", ``Schematic tables are a new representation for conditionals. Roughly a cross between decision tables and data flow graphs, they represent computation and decision-making orthogonally.''"))
  )
