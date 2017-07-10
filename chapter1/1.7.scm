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

; for certain number which is very small
; determination on whether guess's squasre smaller than c plus x mostly depends on c, not x
; for certain number which is very large
; guess would also be large, so guess as a float number scatters
; the absolute value of guess's square minus x would always be larger than c
