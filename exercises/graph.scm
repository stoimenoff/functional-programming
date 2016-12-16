; Добавя (k v) в асоциативния списък a, ползвайки set-cdr!.
(define (insert! k v a)
  (let ((existing (assoc k a)))
    (if existing
      (set-cdr! existing
        (cons v (cdr existing)))
      (set-cdr! a
        (cons (list k v) (cdr a))))))

(define (create-graph vertices)
  (map (lambda (v) (list v '()))
       vertices))

(define (add-vertex! v g) (insert! v '() g))

(define (add-edge! a b g)
  (let ((start-vertex (assoc a g)))
    (set-cdr! start-vertex
              (list (cons b (cadr start-vertex))))))

(define (vertices g) (map car g))

(define (neighbours v g) (cadr (assoc v g)))
