;; (load "1.21.scm")

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
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

;; (search-for-primes 1000)....1000 10000 10000 too samll, almost no time usage so the display is 0.
;; (search-for-primes 1e10) time usage is roughly 3 times of 1e9, nearly (sqrt 10)
;; could be concluded that time usage is proportional to running steps
