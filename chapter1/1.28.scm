(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((= (remainder (square base) m) 1) 0)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test a n)
  (cond ((= a (- n 1)) true)
	((= (expmod a (- n 1) n) 1) (fermat-test (+ a 1) n))
	((not (= (expmod a (- n 1) n) 1)) false)))

(define (fast-prime? n)
  (fermat-test 2 n))

;; carmichael numbers proved false
;; passed
