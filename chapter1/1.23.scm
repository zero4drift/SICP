(define (next n)
  (if (= n 2)
      (+ n 1)
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

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
	((= (timed-prime-test n) 1) (begin (set! count (- count 1)) (search-for-primes-count (+ n 2) count)))
	((= (timed-prime-test n) 0) (search-for-primes-count (+ n 2) count))))

(define (search-for-primes-next n)
  (search-for-primes-count n 3))

;; the ratio is roughly 1.5
;; why?
;; Offical answer: This is mainly due to the NEXT procedure's IF test. The input did halve indeed, but we need to do an extra IF test.
