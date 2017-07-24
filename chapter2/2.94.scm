;; install polynomial package
(define (reamainder-terms t1 t2)
  (cadr (div-terms t1 t2)))

(define (gcd-poly p1 p2)
  (let ((v1 (variable p1))
	(v2 (variable p2))
	(t1 (term-list p1))
	(t2 (term-list p2)))
    (if (same-vaiable? v1 v2)
	(make-poly
	 v1
	 (gcd-terms t1 t2))
	(error "Polys not in same variable -- GCD POLY"
	       (list p1 p2)))))

(put 'greatest-common-divisor
     '(polynomial polynomial)
     (lambda (a b) (tag (gcd-poly a b))))

;; install scheme-number package
(put 'greatest-common-divisor
     '(scheme-number scheme-number)
     gcd)


(define (greatest-common-divisor n d)
  (apply-generic 'greatest-common-divisor n d))
