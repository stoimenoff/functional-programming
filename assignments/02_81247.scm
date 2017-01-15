(define (transpose matrix)
  (if (null? (car matrix))
  '()
  (cons (map car matrix) (transpose (map cdr matrix)))))

(define (reverse-rows matrix)
	(map reverse matrix))

(define (matrix-flatten matrix)
	(fold-right append '() matrix))

(define (matrix-numbers matrix)
	(matrix-flatten (reverse (reverse-rows (transpose matrix)))))
