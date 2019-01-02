(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
	  x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (cv x)
  (let ((y (make-connector)))
    (constant x y)
    y))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))
