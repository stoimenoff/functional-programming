(define (last-digit n)
  (remainder n 10))

(define (all-but-last-digit n)
  (quotient n 10))

(define (append-digit digit number)
  (+ (* number 10) digit))

(define (digit? n)
  (< (abs n) 10))

(define (count-digits-positive n)
  (if (digit? n)
   1
   (+ 1 (count-digits-positive (all-but-last-digit n)))))

(define (count-digits n)
  (count-digits-positive (abs n)))

(define (sum-digits-positive n)
  (if (digit? n)
   n
   (+ (last-digit n) (sum-digits-positive (all-but-last-digit n)))))

(define (sum-digits n)
  (sum-digits-positive (abs n)))

(define (prime-helper n d)
  (cond
    ((= n d) #t)
    ((= 0 (remainder n d)) #f)
    (else (prime-helper n (+ d 1)))
  )
)

(define (prime n)
  (prime-helper (abs n) 2))

(define (prime2 n)
  (define (prime-helper d)
    (cond
      ((= n d) #t)
      ((= 0 (remainder n d)) #f)
      (else (prime-helper n (+ d 1)))
     )
    )
  (prime-helper 2))

(define (automorphic-positive? n)
  (= n (remainder (expt n 2) (expt 10 (count-digits n)))))

(define (automorphic? n)
  (automorphic-positive? (abs n)))

(define (count-digits-iter n)
  (define (helper remaining-digits current-counter)
    (if (digit? remainng-digits)
        (+ 1 current-counter)
        (helper (all-but-last-digit remaining-digits) (+ current-counter 1)))
    )
  (helper n 0)
  )

(define (sum-digits-iter n)
  (define (helper remaining-digits current-sum)
    (if (digit? remainng-digits)
        (+ remaining-digits current-sum)
        (helper (all-but-last-digit remaining-digits) (+ current-sum (last-digit remaining-digits))))
    )
  (helper n 0)
  )

(define (my-reverse n)
  (define(reverse-help current reversed)
    (if (digit? current)
        (append-digit current reversed)
        (reverse-help (all-but-last-digit current) (append-digit (last-digit current) reversed))))
  (reverse-help n 0))

(define (palindrom? n)
  (= n (my-reverse n)))