(define (factorial num)
  (if (< num 2)
      1
      (* num (factorial (- num 1)))))

(define (successor n)
  (+ n 1))

(define (double n)
  (* n 2))

(define (sine x acc)
  (define (sine-taylor-member n)
    (* (expt -1 n)
       (/ (expt x (successor (double n)))
          (factorial (successor (double n))))))

  (define (accurate-enough? member-of-series)
    (< (abs member-of-series) acc))

  (define (sine-helper x acc counter)
    (if (accurate-enough? (sine-taylor-member counter))
      0
      (+ (sine-taylor-member counter) (sine-helper x acc (successor counter)))))
  (sine-helper x acc 0))