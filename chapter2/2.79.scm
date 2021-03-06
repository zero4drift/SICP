;; below three separate statements better put inside install procedure
(define (equ-number? n1 n2)
  (= n1 n2))
(put 'equ? '(scheme-number scheme-number) equ-number?)

(define (equ-rational? r1 r2)
  (let ((product1 (* (numer r1) (denom r2)))
	(product2 (* (denom r1) (numer r2))))
    (= product1 product2)))
(put 'equ? '(rational rational) equ-rational?)

(define (equ-complex? c1 c2)
  (let ((real1 (real-part c1))
	(imag1 (imag-part c1))
	(real2 (real-part c2))
	(imag2 (imag-part c2)))
    (and (= real1 real2) (= imag1 imag2))))
(put 'equ? '(complex complex) equ-complex?)

;; general equ? interface
(define (equ? x y)
  (apply-generic 'equ? x y))
