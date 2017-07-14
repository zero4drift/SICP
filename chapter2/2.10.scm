(define (div-interval x y)
  (let ((upper (upper-bound y))
	(lower (lower-bound y)))
    (if (or (and (< lower 0) (> upper 0)) (= lower 0) (= upper 0))
	(error "the second interval acrossing the zero point")
	(mul-interval x
		      (make-interval (/ 1.0 (upper-bound y))
				     (/ 1.0 (lower-bound y)))))))
