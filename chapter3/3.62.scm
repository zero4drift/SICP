;; (load "./ 3.61.scm")

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "element 0 in the second stream")
      (scale-stream (mul-series
		     s1
		     (calculate-x (scale-stream s2 (/ 1 (stream-car s2)))))
		    (/ 1 (stream-car s2)))))

(define tan-series
  (div-series sine-series cosine-series))
