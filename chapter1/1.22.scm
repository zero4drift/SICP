;; (load "1.21.scm")

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      (start-prime-test (+ n 2) start-time)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n)
  (if (= (remainder n 2) 0)
      (search-for-primes (+ n 1))
      (timed-prime-test n)))

;; (search-for-primes 1000)....1000 10000 10000 too samll, almost no time usage so the display is 0.
;; (search-for-primes 1e9) time usage is about 6 times of 1e10, not (sqrt 10)
;; and 1e11 usage is 2.3 times of 1e10
;; depends on computer?
