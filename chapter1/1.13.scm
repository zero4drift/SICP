(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (good-enough? guess old-guess)
  (< (/ (abs (- guess old-guess)) guess) 0.001))


(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (sqrt-iter (improve guess x)
		 guess
		 x)))
  
(define (sqrt x)
  (sqrt-iter 1.0 0 x))

(define number (/ (+ 1 (sqrt 5)) 2))

(define (number-n n)
  (/ (expt number n) (sqrt 5)))

(define (compare n)
  (- (fib n) (number-n n)))
