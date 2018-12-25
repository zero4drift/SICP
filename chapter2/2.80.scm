;; below three separate statements better put inside install procedure
(define (=zero-number? n)
  (= n 0))
(put '=zero? '(scheme-number) =zero-number?)

(define (=zero-rational? r)
  (= (numer r) 0))
(put '=zero? '(rational) =zero-rational?)

(define (=zero-complex? c)
  (let ((real (real-part c))
	(imag (imag-part c)))
    (and (= real 0) (= imag 0))))
(put '=zero? '(complex) =zero-complex?)

;; general =zero? interface
(define (=zero? x)
  (apply-generic '=zero? x))
