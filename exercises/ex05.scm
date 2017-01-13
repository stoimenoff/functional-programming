(define (member? element list)
  (cond
    ((null? list) #f)
    ((= element (car list)) #t)
    (else (member? element (cdr list)))))

(define (my-length list)
  (if(null? list)
     0
     (+ 1 (my-length (cdr list)))))

(define (my-length-iter list)
  (define (len-help list current)
    (if(null? list)
     current
     (len-help (cdr list) (+ 1 current))))
  (len-help list 0))

(define (nth l n)
  (if(= n 0)
     (car l)
     (nth (cdr l) (- n 1))))

(define (reverse-and-append l1 l2)
  (if(null? l1)
     l2
     (reverse-and-append (cdr l1) (cons (car l1) l2))))

(define (my-reverse list)
  (reverse-and-append list '()))

(define (my-append l1 l2)
  (reverse-and-append (reverse l1) l2))