(load "ex9.scm")

(define g (create-graph '(1 2 3)))
(add-edge! 1 2 g)
(add-edge! 1 3 g)
(add-edge! 3 1 g)
(add-edge! 2 1 g)

(display g)

(degree-in 1 g)
(degree-out 1 g)

(degree-in 2 g)
(degree-out 2 g)

(edges g)
(symetric? g)
(inverted g)
