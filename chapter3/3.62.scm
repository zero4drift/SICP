(define (reciprocal s)
  (cons-stream
   (/ 1 (stream-car s))
   (scale-stream (mul-series (stream-cdr s) (reciprocal s)) (- (/ 1 (stream-car s))))))

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "element 0 in the second stream")
      (mul-series
       s1
       (reciprocal s2))))

(define tan-series
  (div-series sine-series cosine-series))
