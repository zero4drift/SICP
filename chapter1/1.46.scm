(define (iterative-improve good-enough? improve)
  (lambda (x)
    (if (good-enough? x)
	x
	((iterative-improve good-enough? improve) (improve x)))))

;; sqrt

(define (average x y)
  (/ (+ x y) 2))
  
(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  ((iterative-improve good-enough? improve) 1.0))

;; fixed-point

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))

  (define (improve guess)
    (f guess))

  ((iterative-improve close-enough? improve) first-guess))
