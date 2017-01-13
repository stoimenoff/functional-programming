(load "ex8.scm")
(define t
'(1
	(2
		()
		())
	(3
		(4
			()
			())
		())))
(pre-order t)
(in-order t)
(post-order t)
(level 0 t)
(level 1 t)
(level 2 t)
(level 3 t)
(level 4 t)
(display t)
(map-tree odd? t)
(map-tree even? t)

(display t)
(contains-path? '(1 2) t)
(contains-path? '(1 3 4) t)
(contains-path? '(1 3) t)
(contains-path? '(2 3) t)
(contains-path? '(3 4) t)

(paths t)
