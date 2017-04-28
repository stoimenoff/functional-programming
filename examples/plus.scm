(define (prepend-zero l)
	(cons 0 l))

(define (plus-for-list l)
	(* -1 (apply - (prepend-zero l))))

(define (+ . args)
	(if (= 0 (length args))
		1
		(plus-for-list args)))
