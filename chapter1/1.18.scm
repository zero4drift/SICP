(define (double x)
  (+ x x))

(define (halve x)
  (if (=  (remainder x 2) 0)
      (/ x 2)
      (error "Invalid number")))

(define (even? x)
  (= (remainder x 2) 0))

(define (multi-iter x y)
  (cond ((or (= x 0) (= y 0)) 0)
	((= y 1) x)
	((even? y) (multi-iter (double x) (/ y 2)))
	(else (+ x (multi-iter x (- y 1))))))

(define (* x y)
  (multi-iter x y))
