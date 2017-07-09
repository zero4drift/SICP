(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))				;under applicative order this leads to dead loop when eval (p)
					;under normal order this return 0 as result

