;; a
;; fuck it

;; b

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter n))
	    (right (right-split painter n)))
	(beside (below painter up)
		(below (right (corner-split painter (- n 1))))))))

;; c

(define (square-limit painter n)
  (let ((combine4 ((square-of-four flip-vert rotate180
				   identity flip-horiz))))
    (combine4 (corner-split painter n))))
