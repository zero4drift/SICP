(define (count-pairs x)
  (let ((checked '()))
    (define (recursive x)
      (cond ((memq x checked) 0)
	    ((not (pair? x)) 0)
	    (else
	     (begin
	       (set! checked (cons x checked))
	       (+ (recursive (car x))
		  (recursive (cdr x))
		  1)))))
    (recursive x)))
