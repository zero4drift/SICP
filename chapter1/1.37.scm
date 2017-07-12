;; 1.6180...

;; recursive

(define (cont-frac n d k)
  (define (recursive count)
    (if (> count k)
	(/ (n count) (d count))
	(/ (n count) (+ (d count) (recursive (+ count 1))))))
  (recursive 0))

(/ 1 (cont-frac (lambda (i) 1.0)
		(lambda (i) 1.0)
		10))			; generates 1.618055... after 10 steps

;; iterative

(define (cont-frac-iter n d k)
  (define (iter count result)
    (if (= count k)
	result
	(iter (+ count 1)
	      (/ (n count)
		 (+ (d count) result)))))
  (iter 0 0))

(/ 1 (cont-frac-iter (lambda (i) 1.0)
		     (lambda (i) 1.0)
		     10))		;also requires 10 steps
