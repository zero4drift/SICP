(define (install-minus)
  (define (minus-scheme-number n)
    (make-scheme-number (- n)))

  (define (minus-rational n)
    (make-rational (minus (numer n)) (denom n)))

  (define (minus-complex n)
    (make-complex-from-real-imag (minus (real-part n)) (minus (imag-part n))))

  (define (minus-poly n)
    (define (recursive-t terms)
      (if (empty-termlist? terms)
	  the-empty-termlist
	  (let ((first (first-term terms)))
	    (adjoin-term (make-term
		   (order term)
		   (minus (coeff term)))
		  (recursive-t (rest-terms terms))))))
    (make-poly (variable n) (recursive-t (term-list n))))

  (put 'minus 'scheme-number minus-scheme-number)
  (put 'minus 'rational minus-rational)
  (put 'minus 'complex minus-complex)
  (put 'minus 'polynomial minus-poly))

(define (minus n)
  (apply-generic 'minus n))

(define (sub-poly p1 p2)
  (add-poly p1 (minus p2)))

;; add this into install-polynomial-package
(put 'sub '(polynomial polynomial)
     (lambda (x y) (tag (sub-poly x y))))
