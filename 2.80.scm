(define (=zero-number? n)
  (= n 0))

(define (=zero-rational? r)
  (= (numer r) 0))

(define (=zero-complex? c)
  (let ((real (real-part c))
	(imag (imag-part c)))
    (and (= real 0) (= imag 0))))

(put 'equ? 'scheme-number =zero-number?)
(put 'equ? 'rational =zero-rational?)
(put 'equ? 'complex =zero-complex?)

(define (=zero? x)
  (apply-generic '=zero? x))
