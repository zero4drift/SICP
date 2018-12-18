(define (interval-width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (width-accumulated? combiner interval1 interval2)
  (let ((width1 (interval-width interval1))
	(width2 (interval-width interval2))
	(combined-width (interval-width (combiner interval1 interval2))))
   (= combined-width (+ width1 width2))))
