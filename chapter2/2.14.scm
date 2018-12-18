(define (part1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (part2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;; when procedure make-cent-percent and a very small amount percent are applied to construct a interval,
;; the results of both procedures defined before applied to such intervals are approaching the same.
