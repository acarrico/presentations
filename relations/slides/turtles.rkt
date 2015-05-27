#lang slideshow

(require slideshow/code)
(require "../../racket-part1/helpers.rkt")

(provide turtles)

(define (turtles)

  (slide
   #:title #f
   #:name "tortoise hat"
   (bitmap "images/tortoise-hat.jpg"))

  (slide
   #:title "First Cause"

   (para "``If everything must have a cause, then God must have a cause. If there can be anything without a cause, it may just as well be the world as God...''")

   (when-who-what "1927" "Bertrand Russell" "Why I Am Not A Christian")

   (para "``It is exactly of the same nature as the Hindu's view, that the world rested upon an elephant and the elephant rested upon a tortoise; and when they said, 'How about the tortoise?' the Indian said, 'Suppose we change the subject.' ''") )

  (slide
   #:title "You're very clever, but it's turtles all the way down!"

   (vc-append
    20
    (hc-append
     20
     (vr-append
      20
      (blue (ht-append (t "Douglas Hofstadter, ") (it "Gödel, Escher, Bach")))
      (blue (ht-append (t "Stephen Hawking, ") (it "A Brief History of Time")))
      (blue (ht-append (t "Lev Grossman, ") (it "The Magicians"))))
     (bitmap "images/sky-castle.jpg"))
    (para "``The study of magic is not a science, it is not an art,"
          "and it is not a religion. Magic is a craft.''")
    (bitmap "images/bees-keys.jpg")))

  (slide
   #:title "Programming Should Eat Itself"

   (blue (para "Nada Amin, Strange Loop 2014," (it "Programming to understand programming.")))
   (para #:align 'center (bitmap "images/Nada Amin.jpg"))
   (para "Kenichi Asai's reflective programming language Black")
   #;https://github.com/readevalprintlove/black
   )

  )
