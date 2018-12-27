;; only for polynomial with integer coeffs.

;; a
(define (pseudoremainder-terms a b)
  (let ((f1 (first-term a))
	(f2 (first-term b)))
    (let ((o1 (order f1))
	  (o2 (order f2))
	  (c (coeff f2)))
      (let ((constant (expt c (+ 1 o1 (- o2)))))
	(let ((term (make-term 0
			       constant)))
	  (cadr (div-terms
		 (mul-term-by-all-terms term a)
		 b)))))))
	       

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (pseudoremainder-terms a b))))

;; b
(define (gcd-terms-coeff terms)
    (let ((coeffs (map (lambda (t) (coeff t)) terms)))
      (define (recursive coeffs)
	(if (null? (cddr coeffs))
	    (greatest-common-divisor
	     (car coeffs)
	     (cadr coeffs))
	    (greatest-common-divisor
	     (car coeffs)
	     (recursive coeffs))))
      (recursive coeffs)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      (let ((coeffs-gcd (gcd-terms-coeff a)))
	(div-terms
	 a
	 (list (make-term 0 coeffs-gcd))))
      (gcd-terms b (pseudoremainder-terms a b))))
