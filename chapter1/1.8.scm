(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (good-enough? guess old-guess)
  (< (/ (abs (- guess old-guess)) guess) 0.000001))

(define (square x)
  (* x x))

(define (improve-guess guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(define (cubic-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (cubic-iter (improve-guess guess x)
		  guess
		  x)))

(define (cubic x)
  (cubic-iter 1.0 0 x))
