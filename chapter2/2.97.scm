;; a

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

(define (simplify terms)
  (let ((g (gcd-terms-coeff terms)))
    (div-terms
     terms
     (list (make-term 0 g)))))

(define (reduce-terms n d)
  (list (simplify n) (simplify d)))	;the requirement statements in the book of chinese verson is vague
;; so I just make the first answer like this to keep it different from the sencond

(define (reduce-poly p1 p2)
  (let ((v1 (variable p1))
	(v2 (variable p2))
	(t1 (term-list p1))
	(t2 (term-list p2)))
    (let ((result (reduce-terms n d)))
      (if (same-vaiable? v1 v2)
	  (list (make-poly v1 (car result))
		(make-poly v1 (cadr result)))
	  (error "Polys not in same variable -- REDUCE-POLY"
		 (list p1 p2))))))

;; b

(define (reduce-terms-like n d)
  (let ((g (gcd-terms n d)))
    (list (div-terms n g) (div-terms d g))))

(define (reduce-poly p1 p2)
  (let ((v1 (variable p1))
	(v2 (variable p2))
	(t1 (term-list p1))
	(t2 (term-list p2)))
    (let ((result (reduce-terms-like n d)))
      (if (same-vaiable? v1 v2)
	  (list (make-poly v1 (car result))
		(make-poly v1 (cadr result)))
	  (error "Polys not in same variable -- REDUCE-POLY"
		 (list p1 p2))))))
(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(put 'reduce '(scheme-number scheme-number)
     reduce-integers)

(put 'reduce '(polynomial polynomial)
     reduce-poly)

(define (reduce n d)
  (apply-generic 'reduce n d))
