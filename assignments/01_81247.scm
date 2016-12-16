(define (digit? n)
  (< n 10))

(define (last-digit n)
  (remainder n 10))

(define (all-but-last-digit n)
  (quotient n 10))

(define (reverse-digits-list n)
  (if (digit? n)
      (cons n '())
      (cons (last-digit n) (reverse-digits-list (all-but-last-digit n)))))

(define (sum-digits p n)
  (fold-left + 0 (filter p (reverse-digits-list n))))
