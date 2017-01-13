(define (generate-list n)
  (if (= n 0)
      '()
      (cons n (generate-list (- n 1)))))

(define (member? element structure)
  (cond
    ((null? structure) #f)
    ((equal? element (car structure)) #t)
    ((and (list? (car structure)) (member? element (car structure))) #t)
    (else (member? element (cdr structure)))))


(define (flatten structure)
  (cond
    ((null? structure) '())
    ((list? (car structure)) (append (flatten (car structure)) (flatten (cdr structure))))
    (else (cons (car structure) (flatten (cdr structure))))))


(define (transpose matrix)
  (if (null? (car matrix))
  '()
  (cons (map car matrix) (transpose (map cdr matrix)))))


(define (filter p l)
  (define (helper current-l result)
    (cond
      ((null? current-l) result)
      ((p (car current-l)) (helper (cdr current-l)
                                   (append result (list (car current-l)))))
      (else (helper (cdr current-l) result))))
  (helper l '()))


(define (member? element list)
  (cond
    ((null? list) #f)
    ((= element (car list)) #t)
    (else (member? element (cdr list)))))

(define (union l1 l2)
  (cond
    ((null? l1) l2)
    ((member? (car l1) l2) (union (cdr l1) l2))
    (else (union (cdr l1) (cons (car l1) l2)))))

(define (union-2 l1 l2)
  (append l1 (filter (lambda (x) (not (member? x l1))) l2)))

(define (intersection l1 l2)
  (cond
    ((null? l1) '())
    ((member? (car l1) l2) (cons (car l1) (intersection (cdr l1) l2)))
    (else (intersection (cdr l1) l2))))

(define (intersection-2 l1 l2)
  (filter (lambda (x) (member x l1)) l2))

(define (difference l1 l2)
  (cond
    ((null? l1) '())
    ((member? (car l1) l2) (difference (cdr l1) l2))
    (else (cons (car l1) (difference (cdr l1) l2)))))

(define (difference-2 l1 l2)
  (filter (lambda (x) (not (member x l2))) l1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 0)
    (lambda (x) x)
    (compose f (repeat f (- n 1)))))

(define (compose-n f n)
  (if (= 1 n)
      f
      (compose f (compose-n f (- n 1)))))

(define (compositions l f)
  (map (lambda (x) (compose-n f x)) l))

(define (construct-list n)
  (if (= 0 n)
      '()
      (cons (generate-list n) (construct-list (- n 1)))))

(define (construct-list-2 n)
  (map generate-list (generate-list n)))

