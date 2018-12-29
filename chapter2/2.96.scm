;; only for polynomial with integer coeffs.

;; inside install saprse procedure
(define (coeffs-s terms)
  (map (lambda (t) (coeff t)) terms))
(put 'coeffs '(polynomial-sparse) coeffs-s)

;; inside install dense procedure
(define (coeffs-d terms)
  (cond ((empty-termlist-d? terms) '())
	((equ? (car terms) 0) (coeffs-d (cdr terms)))
	(else (cons (car terms) (coeffs-d (cdr terms))))))
(put 'coeffs '(polynomial-dense) coeffs-d)

;; generic operation
(define (coeffs t)
  (apply-generic 'coeffs t))

;; a
(define (pseudoremainder-terms a b)
  (let ((f1 (first-term a))
	(f2 (first-term b)))
    (let ((o1 (order f1))
	  (o2 (order f2))
	  (c (coeff f2)))
      (let ((constant (exp c (+ 1 o1 (- o2)))))
	(let ((term (make-term 0
			       constant)))
	  (cadr (div-terms
		 (mul-term-by-all-terms term a)
		 b)))))))

;; b
(define (gcd-terms-coeff l)
  (define (recursive l)
    (cond ((= 2 (length l))
	   (greatest-common-divisor (car l) (cadr l)))
	  ((= 1 (length l)) (car l))
	  ((null? l) 0)
	  (else (greatest-common-divisor
		 (car l)
		 (recursive (cdr l))))))
  (recursive l))
(define (gcd-terms a b)
  (if (empty-termlist? b)
      (let ((coeffs-gcd (gcd-terms-coeff (coeffs a))))
	(car (div-terms
	      a
	      (make-terms (type-tag a)
			  (list (make-term 0 coeffs-gcd))))))
      (gcd-terms b (pseudoremainder-terms a b))))
