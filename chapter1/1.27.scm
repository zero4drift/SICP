(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (full-fermat-test n)
  (define (iter-test n a)
    (if (< a n)
	(if (not  (= (expmod a n n) a))
	    (display "False")
	    (iter-test n (+ a 1)))
	(display "True")))
  (iter-test n 2))

;; This will do
