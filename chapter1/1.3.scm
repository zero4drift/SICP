(define (max-sum a b c)
  (cond ((and (>= a b) (>= c b)) (+ a c))
	((and (>= a c) (>= b c)) (+ a b))
	(else (+ b c))))

(max-sum 1 2 3)				;5
(max-sum 3 2 1)				;5
(max-sum 3 1 2)				;5
