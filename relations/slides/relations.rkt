#lang slideshow

(require slideshow/code)
(require "../../racket-part1/helpers.rkt")

(provide relations)

(define (relations)

  (slide
   #:title (when-who-what "2002" "Benjamin Pierce" "Types and Programming Languages")
   #:name "tapl"
   (bitmap "images/tapl0.jpg")
   )

  (slide
   #:title "tapl relation"
   (scale-to-fit (bitmap "images/tapl-relation.jpg") 1000 700))

  (slide
   #:title "Bertrand Russell (1872-1970)"
   (when-who-what "1910-1913" "Alfred North Whitehead, Bertrand Russell" "Principia Mathematica")
   (para #:align 'center (bitmap "images/Bertrand_Russell.png"))

   #;(para "PM emphasizes relations as a fundamental concept, whereas in current mathematical practice it is functions rather than relations that are treated as more fundamental")
   )

  (slide
   #:title "Gottlob Frege (1848-1925)"
   (item "foundation of math in logic")
   (item "axiomatic, formal language")
   (item "propositional logic")
   (item "predicate logic, quantifiers")
   (bitmap "images/Young_frege.jpg"))

  (slide
   #:title "Declarative Sentence"
   (para "``"(red (it "Is it true that ")) (bit "x") (red (t " ?")) "''")
   (when-who-what "1977" "Wilfred Hodges" "Logic"))

  (slide
   #:title "Proposition"
   (red
    (vc-append
     5
     (tt "2 < 10")
     (tt "20 < 10")))

   (para "``A proposition is a formula that is necessarily true or false, but cannot be both.''")
   (when-who-what "1990, 2003" "Frank Brown" "Boolean Reasoning")

   (item "compare: " (red (tt "1 + 2")))
   (item "truth-table")
   (item "propositional calculus"))

  (slide
   #:title "Predicate"

   (red
    (vc-append
     5
     (tt "2 < 10")
     (tt "x + 1 < 10")
     (tt "x + y < 10")
     (tt "x + y < z")))

   (para "``the formula represented by " (tt "P(x1, ..., xn)") " is an " (it "n-variable predicate") "if it becomes a proposition for each allowable substitution of values for " (tt "x1, ..., xn") "''")
   (when-who-what "1990, 2003" "Frank Brown" "Boolean Reasoning")

   (item "Satisfaction")
   (item "Situation: " (red (it "x")) (it "is at least 2 years old."))
   (item "Quantification"))

  (slide
   #:title "Relation (Frank Brown)"
   (bitmap "images/brown-relation.jpg")

   (item "Brown does extend this to three or more sets")
    )

  (slide
   #:title "Relation"

   (para "``There are two main ways to describe a relation.''")

   (item "listing the n-tuples which are in it.")
   (item "all of the ordered n-tuples which satisfy an n-place predicate")

   (para "``The relation is said to be expressed by the predicate''")

   (when-who-what "1977" "Wilfred Hodges" "Logic")

   )

  (slide
   #:title "tapl relation"
   (scale-to-fit (bitmap "images/tapl-relation.jpg") 1000 700)

   (item "partitioning"))

  (slide
   #:title "tapl predicate"
   (scale-to-fit (bitmap "images/tapl-predicate.jpg") 1000 700))

  (slide
   #:title "tapl binary relation"
   (scale-to-fit (bitmap "images/tapl-binary-relation.jpg") 1000 700))

  (slide
   #:title "tapl mixfix"
   (scale-to-fit (bitmap "images/tapl-mixfix.jpg") 1000 700))

  (slide
   #:title "Shapes"
   (item "reflexive, symmetric, transitive")
   (item "equivalence")
   (item "partial-order")
   (item "one-to-one, onto, injection, bijection, surjection")
   (item "function")
   )

  (slide
   #:title "Multiset Discrimination"

   (para "``finding duplicates in linear time without hashing or comparison-based sorting.''")

   (when-who-what "2003" "Fritz Henglein" "Multiset Discrimination")
   (item "Robert Paige")
   (item "acyclic (atoms, references, structured values (ordered, sets, bag/multiset): worst case linear time")
   (item "cyclic stores: worst case time O(m log n) space O(m) for m edges, n nodes")
   (item "sorting in linear time, with a sorting guide"))

  (slide
   #:title "Edgar Codd (1923-2003)"

   (when-who-what "1970" "Edgar Codd" "A Relational Model of Data for Large Shared Data Banks")

   (bitmap "images/Edgar_F_Codd.jpg")

   (when-who-what "2011" "Fritz Henglein" "Generic Multiset Programming with Discrimination-based joins and symbolic Cartesian products."))

  (slide
   #:title #f
   #:name "Reasoned Schemer"
   (scale-to-fit (bitmap "images/reasoned.jpg") 1000 700))

  (slide
   #:title #f
   #:name "Van Roy"
   (scale-to-fit (bitmap "images/Van Roy.jpg") 1000 700))

  )
