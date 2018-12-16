(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime n (- (runtime) start-time))
      0))				;0 as a return value indicates failure

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  1)					;1 as a return value indicates success

(define (search-for-primes-count n count)
  (cond ((= (remainder n 2) 0) (search-for-primes-count (+ n 1) count))
	((= count 0) 'ok)
	((= (timed-prime-test n) 1) (search-for-primes-count (+ n 2) (- count 1)))
	((= (timed-prime-test n) 0) (search-for-primes-count (+ n 2) count))))

(define (search-for-primes n)
  (search-for-primes-count n 3))

;; time usage of calculating on 1e24 is two times of 1e12
;; so it's truly logarithmic growth
