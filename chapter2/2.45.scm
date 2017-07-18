(define (split arrangement1 arrangement2)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split arrangement1 arrangement2) painter (- n 1))))
	  (arrangement1 painter (arrangement2 smaller smaller))))))
