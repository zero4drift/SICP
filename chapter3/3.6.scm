(define rand
  (let ((x random-init))
    (define (generate)
      (begin (set! x (rand-update x))
	     x))
    (define (reset n)
      (set! x n))
    (define (dispatch s)
      (cond ((eq? s 'generate)
	     (generate))
	    ((eq? s 'reset)
	     reset)
	    (else (error "Unknown request -- META-ACCOUNT"
			 s))))
    dispatch))