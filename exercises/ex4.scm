(define (succ x)
  (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))


(define (delta-y f x1 x2)
  (- (f x1) (f x2)))

(define (lim f x0)
  (define x (+ x0 (/ 10 1000000000000000000)))
  (/ (delta-y f x x0) (- x x0)))

(define (derivative f)
  (lambda (x0)
    (lim f x0)))


(define pi 3.14159265)

(define (repeat f n)
  (if (= n 0)
    (lambda (x) x)
    (compose f (repeat f (- n 1)))))

(define (derive-n f n)
    ((repeat derivative n) f))
