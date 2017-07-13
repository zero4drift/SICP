(define (inc n)
  (+ n 1))

(define (compose f1 f2)
  (lambda (x)
    (f1 (f2 x))))

((compose square inc) 6)
