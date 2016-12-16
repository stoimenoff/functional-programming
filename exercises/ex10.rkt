(define (enum start end)
	(if (> start end)
		empty-stream
		(stream-cons start (enum (+ 1 start) end))))

(define (stream-to-list s)
	(if (stream-empty? s)
	'()
	(cons (stream-first s) (stream-to-list (stream-rest s)))))

(define (list-to-stream l)
	(if (null? l)
	empty-stream
	(stream-cons (car l) (list-to-stream (cdr l)))))

(define (stream-take s n)
	(if (or (stream-empty? s) (= n 0))
	'()
	(cons (stream-first s) (stream-take (stream-rest s) (- n 1)))))

(define (repeat n)
	(stream-cons n (repeat n)))

(define (sequence start)
	(stream-cons start (sequence (+ 1 start))))

(define (add-streams s1 s2)
	(if(or (stream-empty? s1) (stream-empty? s2))
		empty-stream
		(stream-cons (+ (stream-first s1) (stream-first s2))
			(add-streams (stream-rest s1) (stream-rest s2)))))

(define (natural)
	(sequence 1))

(define (fib-helper first second)
	(stream-cons first (fib-helper second (+ first second))))

(define (fibonaccis)
	(fib-helper 1 1))

(define (prime-helper n d)
  (cond
    ((= n d) #t)
    ((= 0 (remainder n d)) #f)
    (else (prime-helper n (+ d 1)))
  )
)

(define (prime? n)
  (prime-helper (abs n) 2))

(define (primes-helper n)
	(if (prime? n)
		(stream-cons n (primes-helper (+ 1 n)))
		(primes-helper (+ 1 n))))

(define (primes)
	(primes-helper 2))

(define (repeat-list l)
	(define (repeat-list-helper current)
		(if (null? current)
			(repeat-list-helper l)
			(stream-cons (car current) (repeat-list-helper (cdr current)))))
	(repeat-list-helper l))

(define (repeat-switch-lists l1 l2)
	(define (repeat-list1-helper current1 current2)
		(if (null? current1)
			(repeat-list1-helper l1 current2)
			(stream-cons (car current1) (repeat-list2-helper (cdr current1) current2))))
	(define (repeat-list2-helper current1 current2)
		(if (null? current2)
			(repeat-list2-helper current1 l2)
			(stream-cons (car current2) (repeat-list1-helper current1 (cdr current2)))))
	(repeat-list1-helper l1 l2))


(define (repeat-lists l1 l2)
	(define (repeat-list1-helper current)
		(if (null? current)
			(repeat-list2-helper l2)
			(stream-cons (car current) (repeat-list1-helper (cdr current)))))
	(define (repeat-list2-helper current)
		(if (null? current)
			(repeat-list1-helper l1)
			(stream-cons (car current) (repeat-list2-helper (cdr current)))))
	(repeat-list1-helper l1))

(define ())
