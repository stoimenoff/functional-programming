(load "graph.scm")

(define (sum-list l)
    (fold-right + 0 l))

(define (degree-out v g)
    (length (neighbours v g)))

(define (degree-in v g)
    (length (filter (lambda (entry) (member v (cadr entry))) g)))

(define (repeat n element)
    (if (= 0 n)
        '()
        (cons element (repeat (- n 1) element))))

(define (unfold-edges entry)
    (define v (car entry))
    (define neighbours (cadr entry))
    (map list (repeat (length neighbours) v) neighbours))

(define (edges g)
    (fold-right append '() (map unfold-edges g)))

(define (conjuntion a b)
    (and a b))

(define (all? p l)
    (fold-right conjuntion #t (map p l)))

(define (symetric? g)
    (define (has-symetric? edge)
        (member (reverse edge) (edges g)))
    (all? has-symetric? (edges g)))

(define (invert g)
    (let ((inverted-g (create-graph (vertices g))))
        (begin
            (for-each (lambda (edge) (add-edge! (cdr edge) (car edge) inverted-g)) (edges g))
            (inverted-g))))
