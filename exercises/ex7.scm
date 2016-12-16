(define (compose-two f g)
  (lambda (x) (f (g x))))

(define (compose l)
	(fold-right compose-two (lambda (x) x) l))

(define (sum-list l)
	(fold-right + 0 l))

(define (sum-many-lists ll)
	(sum-list (map sum-list ll)))

(define (sum-lists lists element)
	(sum-many-lists(filter (lambda (l) (member? element l)) lists)))

(define (reverse-sequence n)
	(if (= n 0)
	'()
	(cons n (reverse-sequence (- n 1)))))

(define (sequence n)
	(reverse (reverse-sequence n)))

(define (minus-one x)
	(- x 1))

(define (range n)
	(map minus-one (sequence n)))

(define (reverse-range n)
	(map minus-one (reverse-sequence n)))

(define (repeat n element)
	(if (= 0 n)
		'()
		(cons element (repeat (- n 1) element))))

(define (column i matrix)
	(map list-ref matrix (repeat (length matrix) i)))

(define (l-diagonal matrix)
	(map list-ref matrix (range (length matrix))))

(define (r-diagonal matrix)
	(map list-ref matrix (reverse-range (length matrix))))

(define (filter-matrix p matrix)
	(map (lambda (row) (filter p row)) matrix))

(define (remove-ith i l)
    (define ith-element (list-ref l i))
    (filter (lambda (element) (not (equal? ith-element element))) l))

(define (skip-row i matrix)
    (remove-ith i matrix))

(define (skip-column i matrix)
    (map remove-ith (repeat (length matrix) i) matrix))

(define (transpose matrix)
    (apply map list matrix))
