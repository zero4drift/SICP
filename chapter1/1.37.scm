;; 0.6180...

;; recursive

(define (cont-frac n d k)
  (define (recursive count)
    (if (= count k)
	(/ (n count) (d count))
	(/ (n count) (+ (d count) (recursive (+ count 1))))))
  (recursive 1))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   11)			; generates 0.6180555... after 11 steps

;; iterative

(define (cont-frac-iter n d k)
  (define (iter count result)
    (if (= count k)
	(/ (n count)
	   (+ (d count) result))
	(iter (+ count 1)
	      (/ (n count)
		 (+ (d count) result)))))
  (iter 1 0))

(/ 1 (cont-frac-iter (lambda (i) 1.0)
		     (lambda (i) 1.0)
		     11))		;also requires 11 steps
