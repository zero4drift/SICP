(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-it b n a)
  (cond ((= n 0) a)
	((even? n) (fast-expt-it b (/ n 2) (* a (* b b))))
	(else (* b (fast-expt-it b (/ (- n 1) 2) (* a (* b b)))))))

(define (fast-expt b n)
  (fast-expt-it b n 1))
