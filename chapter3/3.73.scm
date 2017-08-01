(define (RC r c dt)
  (define (voltage i v0)
    (add-streams
     (scale-stream i r)
     (integral (scale-stream i (/ 1 c))
	       v0
	       dt)))
  voltage)
