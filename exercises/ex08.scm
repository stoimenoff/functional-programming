(define (root t) (car t))
(define (left t) (cadr t))
(define (right t) (caddr t))
(define (the-empty-tree) '())
(define (tree-empty? t) (null? t))
(define (make-tree root left right)
	(list root left right))
(define (leaf? t)
	(and (not (tree-empty? t))
		(tree-empty? (left t))
		(tree-empty? (right t))))

(define (pre-order t)
	(if (tree-empty? t)
	'()
	(cons (root t) (append (pre-order (left t)) (pre-order (right t))))))

(define (in-order t)
	(if (tree-empty? t)
	'()
	(append (in-order (left t)) (list (root t)) (in-order (right t)))))

(define (post-order t)
	(if (tree-empty? t)
	'()
	(append (post-order (left t)) (post-order (right t)) (list (root t)))))

(define (level n t)
	(cond
		((tree-empty? t) '())
		((= n 0) (list (root t)))
		(else (append (level (- n 1) (left t)) (level (- n 1) (right t))))))

(define (map-tree f t)
	(if (tree-empty? t)
		(the-empty-tree	)
		(make-tree (f (root t)) (map-tree f (left t)) (map-tree f (right t)))))

; broken
(define (contains-path? p t)
	(cond
		((null? p) #f)
		((tree-empty? t) #f)
		((and (leaf? t) (= (length p) 1)) (= (root t) (car p)))
		((= (root t) (car p))
			 (or (contains-path? (cdr p) (left t))
		 	(contains-path? (cdr p) (right t))))
		(else #f)))

(define (paths t)
	(define (prepend-root l)
		(cons (root t) l))
	(define (prepend-root-to-all ll)
		(map prepend-root ll))
	(cond
		((tree-empty? t) '())
		((leaf? t) (list (list (root t))))
		(else (append (prepend-root-to-all (paths (left t)))
				(prepend-root-to-all (paths (right t)))))))
