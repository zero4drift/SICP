(define (higher-level-cont-frac combiner n d k)
  (define (recursive count)
    (if (= count k)
	(/ (n count) (d count))
	(/ (n count) (combiner (d count) (recursive (+ count 1))))))
  (recursive 1))

(define (tan-cf x k)
  (higher-level-cont-frac -
			  (lambda (i) (if (= i 1) x (square x)))
			  (lambda (i) (- (* i 2) 1))
			  k))
;; test

(define tan60 (/ (sqrt 3) 1))
(define tolerance 0.0001)

(< (abs (- (tan-cf (/ pi 3) 10) tan60)) tolerance) ;passed
