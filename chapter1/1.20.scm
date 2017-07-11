(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)				;under normal order remainder is applied 18 times
					;under applicative order remainder is applied 4 times
