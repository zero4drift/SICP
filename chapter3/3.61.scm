(define (calculate-x s)
  (cons-stream
   1
   (scale-stream (mul-series (stream-cdr s) (calculate-x s)) -1)))
