(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(A 1 10)				;1024
(A 2 4)					;65536
(A 3 3)					;65536

(define (f n)
  (A 0 n))				;(* 2 n)

(define (g n)
  (A 1 n))				;(if (= n 0) 0 (expt 2 n))

(define (h n)
  (A 2 n))				;(cond ((= n 0) 0) ((= n 1) 2) (else (expt 2 (expt 2 (expt 2  (- n 2))))))
