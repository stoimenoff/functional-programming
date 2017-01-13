(define (count-digits n)
  (if (digit? n)
   1
   (+ 1 (count-digits (all-but-last-digit n)))))

(define (last-digit n)
  (remainder n 10))

(define (cut-last-digit n)
  (quotient n 10))

(define (append-digit digit number)
  (+ (* number 10) digit))

(define (digit? n)
  (< (abs n) 10))

(define (successor n)
  (+ 1 n))

(define (predecessor n)
  (- n 1))

(define (my-reverse n)
  (define(reverse-help current reversed)
    (if (digit? current)
        (append-digit current reversed)
        (reverse-help (cut-last-digit current) (append-digit (last-digit current) reversed))))
  (reverse-help n 0))

(define (palindrome? n)
  (define (palindrome?-helper first second)
    (cond
      ((= first second) #t)
      ((< first second) #f)
      ((= (cut-last-digit first) second) #t)
      (else (palindrome?-helper (cut-last-digit first) (append-digit (last-digit first) second)))))
  (if (and (= 0 (last-digit n)) (not (digit? n)))
      #f
      (palindrome?-helper n 0)))

(define (count-palindromes start end)
  (define (counting-helper current-start current-count)
    (cond
      ((> current-start end) current-count)
      ((palindrome? current-start) (counting-helper (successor current-start) (successor current-count)))
      (else (counting-helper (successor current-start) current-count))))
  (counting-helper start 0))

(define (ends-with? n a)
  (cond
    ((< n a) #f)
    ((digit? a) (= (last-digit n) a))
    ((= (last-digit n) (last-digit a)) (ends-with? (cut-last-digit n) (cut-last-digit a)))
    (else #f)))

(define (contains? n a)
  (cond
    ((< n a) #f)
    ((ends-with? n a) #t)
    (else (contains? (cut-last-digit n) a))))

(define (fib-iter n)
  (define (fib-iter-helper previous current index)
    (if (= index n)
        current
        (fib-iter-helper current (+ previous current) (successor index))))
  (if (< n 2)
      1
      (fib-iter-helper 1 1 1)))

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (predecessor n)) (fib (predecessor (predecessor n))))))