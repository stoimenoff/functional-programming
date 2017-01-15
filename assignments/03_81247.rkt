(define (stream-constructor element-at starting-index)
	(stream-cons (element-at starting-index)
				(stream-constructor element-at (+ 1 starting-index))))

(define (factorial n)
	(do ((n n (- n 1))
		(result 1 (* result n)))
		((= n 0) result)))

(define (construct-stream x)
	(define (element-at index)
		(/ (expt x index) (factorial index)))
	(stream-constructor element-at 0))
