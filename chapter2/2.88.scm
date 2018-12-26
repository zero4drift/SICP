(define (install-negative)
  (define (negative-scheme-number n)
    (make-scheme-number (- n)))

  (define (negative-rational n)
    (make-rational (negative (numer n)) (denom n)))

  (define (negative-complex n)
    (make-complex-from-real-imag (negative (real-part n)) (negative (imag-part n))))

  (define (negative-poly n)
    (define (recursive-t terms)
      (if (empty-termlist? terms)
	  the-empty-termlist
	  (let ((first (first-term terms)))
	    (adjoin-term (make-term
			  (order first)
			  (negative (coeff first)))
			 (recursive-t (rest-terms terms))))))
    (make-poly (variable n) (recursive-t (term-list n))))

  (put 'negative '(scheme-number) negative-scheme-number)
  (put 'negative '(rational) negative-rational)
  (put 'negative '(complex) negative-complex)
  (put 'negative '(polynomial) negative-poly))

(define (negative n)
  (apply-generic 'negative n))

(define (sub-poly p1 p2)
  (add-poly p1 (negative p2)))

;; add this into install-polynomial-package
(put 'sub '(polynomial polynomial)
     (lambda (x y) (tag (sub-poly x y))))
