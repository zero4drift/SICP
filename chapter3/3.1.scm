(define (make-accumulator n)
    (lambda (amount)
      (begin (set! n (+ n amount))
	     n)))
