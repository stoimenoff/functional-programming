(define (add a b)
  (+ a b))
(add 2 32)

(define pi 3.14159265)

(add pi 3)


(define (compareToPie x)
  (if (< x pi) "smaller" "greater"))
(compareToPie 5)
(compareToPie 2)

(define (even x)
  (= (remainder x 2) 0)
  )

(define (odd x)
  (not (even x)))

(define (signum x)
  (if (< x 0)
      -1
      (if (> x 0)
          1
          0
          )
      )
  )

(define (signum2 x)
  (cond
    ((< x 0) -1)
    ((= x 0) 0)
    (else 1)
    )
  )

(define (sumrange from to)
  (if (<= from to)
      (+ from (sumrange (+ from 1) to)) 0)
  )

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))
      )
  )

(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))
      )
  )

(define (pow2 x)
  x * x)

(define (fastpower x n)
  (cond
    ((= n 0) 1)
    ((even n) (pow2 (fastpower x (quotient n 2))))
    (else ( * x (fastpower x (- n 1))))
    )
  )
