(define (make-segment x y m n)
  (cons (make-vect x y)
	(make-vect m n)))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
