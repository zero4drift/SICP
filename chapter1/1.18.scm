(define (double x)
  (+ x x))

(define (halve x)
  (if (=  (remainder x 2) 0)
      (/ x 2)
      (error "Invalid number")))

(define (even? x)
  (= (remainder x 2) 0))

(define (multi-iter x y z)
  (cond ((or (= x 0) (= y 0)) 0)
	((and (= y 1) (= z 0)) x)
	((= y 1) z)
	((even? y) (multi-iter x (/ y 2) (+ z (double x))))
	(else (+ x (multi-iter x (- y 1) z)))))

(define (* x y)
  (multi-iter x y 0))
