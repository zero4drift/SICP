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

(define (memq p1 p2)
  (cond ((null? p2) false)
	((equal? p1 (car p2)) p2)
	(else (memq p1 (cdr p2)))))
