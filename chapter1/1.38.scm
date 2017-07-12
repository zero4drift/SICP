(define (compute-next i)
  (define the-count 1)
  (define the-number 2)
  (cond ((= the-count i) the-number)
	((= (remainder (- i the-count) 3) 0)
	 (+ (* (/ (- i the-count) 3) 2) the-number))
	(else 1)))

(define (cont-frac n d k)
  (define (recursive count)
    (if (> count k)
	(/ (n count) (d count))
	(/ (n count) (+ (d count) (recursive (+ count 1))))))
  (recursive 0))

(cont-frac (lambda (i) 1.0) compute-next 10) ;0.718281822... e: 2.718281828...
