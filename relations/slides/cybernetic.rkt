#lang slideshow

(require slideshow/code)
(require "../../racket-part1/helpers.rkt")

(provide cybernetic)

(define (cybernetic)

  (slide
   #:title "This statement is false."
   (para "A" (it "proposition?") "A paradox.")
   (bitmap "images/oscillator.jpg")
   (para "An oscillator."))

  (slide
   #:title "Josiah Willard Gibbs (1839-1903)"
   #:layout 'tall
   #;(blue (para "\"... a system runs through all the distributions of position and momentum which are compatible with its energy...\", Norbert Wiener"))
   (blue (para "``Gibb's innovation was to consider not one world, but all the worlds which are possible answers to a limited set of questions... "
               #;"...concerning our environment.''"
               "His central notion concerned the extent to which answers that we may give to questions about one set of worlds are probable among a larger set of worlds.'', Norbert Wiener"))

   (red (para "``The measure of this probability is called entropy''"))

   (para #:align 'center (bitmap "images/Josiah_Willard_Gibbs.jpg")))

  (slide
   #:title "Norbert Wiener (1894-1964) — Cybernetics"
   #:layout 'tall
   (blue (t "1968ish: Philip Peterson, \"Digital Norbert Wiener\""))
   (para #:align 'center (bitmap "images/Norbert Wiener.jpg")))

  (slide
   #:title "Claude Shannon (1916-2001) — Information Theory"
   (para #:align 'center (bitmap "images/Claude Shannon.jpg"))
   (t "The bit"))

  (slide
   #:title #f
   #:name "Cybernetics"
   #:layout 'tall
   (blue (para "1948: Norbert Wiener, \"Cybernetics—Control and Communication in the Animal and Machine\""))
   (blue (para "1950: Norbert Wiener, \"The Human Use of Human Beings—Cybernetics and Society\""))

   (para #:align 'center (bitmap "images/human use of human beings.jpg")))

  (slide
   #:title "Cybernetics"

   (blue (para "Wiener, ``It is my thesis that the physical functioning of the living individual and the operation of some of the newer communication machines are precisely parallel in the analoguos attempts to control entropy through feedback.''")))

  (slide
   #:title #f
   #:name "Player Piano"
   (blue (t "1952: Kurt Vonnegut, \"Player Piano\""))
   (para #:align 'center (bitmap "images/PlayerPianoFirstEd.jpg")))

  (slide
   #:title #f
   #:name "Yes, My Child"
   (blue (t "1968: Norman Toynton, \"Yes, My Child\""))
   (para #:align 'center (bitmap "images/yes my child.jpg")))

  (slide
   #:title #f
   #:name "Eno"
   (blue (t "1974: Brian Eno, \"Seven Deadly Finns\""))
   (para #:align 'center (bitmap "images/ENO_SEVEN_DEADLY_FINNS_7_MISPRESS.jpg")))

  (slide
   #:title #f
   #:name "Cybermen"
   (blue (t "2013: Chris-Rachael Oesland, \"White Chocolate Cyberman Heads\""))
   (para #:align 'center (bitmap "images/dw-cyberman-heads-chocolate.jpg")))

  (slide
   #:title "Ludwig Wittgenstein (1889-1951), 1921"

   (para #:align 'center (bitmap "images/Tractatus_title_page_1922_Harcourt.png")))

  (slide
   #:title "Ricardo Uribe"
   #:layout 'tall
   (para "\"intending to stimulate in the audience thoughts similar to mine.\"")

   ;;(para "\"Reasoning paradoxically, a group of observers may find tentative common meanings for propositions through the language-games that define their forms of life.\"")

   (para #:align 'center (bitmap "images/Paradoxico.jpg"))
   #;(para #:align 'center (bitmap "images/oro.gif"))

   (blue (para "1991 Yellow, 1994 Blue, Red Edition: http://bcl.ece.illinois.edu/Uribe/index.htm")))

  (slide
   ;;#:title "Ricardo Uribe"
   (para #:align 'center (bitmap "images/Uribe.jpg")))
  )
