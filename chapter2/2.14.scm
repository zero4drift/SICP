(define (part1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (part2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval r2)))))

;; when procedure make-cent-percent  and a very amount percent are applied to construct a interval
;; the result of div-interval applied to such intervals is approaching true