(define (percent-accumulated? x y)
  (let ((px (percent x))
	(py (percent y))
	(pr (percent (mul-interval x y))))
    (= pr (percent-accumulated px py))))

(define (percent-accumulated p1 p2)
  (/ 1 (abs (- (/ 1 p1) (/ 1 p2)))))
