(define (double x)
  (+ x x))

(define (halve x)
  (if (=  (remainder x 2) 0)
      (/ x 2)
      (error "Invalid number")))

(define (even? x)
  (= (remainder x 2) 0))

(define (* x y)
  (cond ((or (= y 0) (= x 0)) 0)
	((= y 1) x)
	((even? y) (* (double x) (/ y 2)))
	(else (+ x (* x (- y 1))))))
