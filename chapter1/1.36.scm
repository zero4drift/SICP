(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)

(define (report-evolution n)
  (newline)
  (display " *** ")
  (display n))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (report-evolution guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2) ;>20 steps

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2) ;only 9 steps on my computer
