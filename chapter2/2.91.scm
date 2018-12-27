(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
	    (t2 (first-term L2)))
	(if (> (order t2) (order t1))
	    (list (the-empty-termlist) L1)
	    (let ((nwe-c (div (coeff t1) (coeff t2)))
		  (new-o (- (order t1) (order t2))))
	      (let ((rest-of-result
		     (div-terms
		      (sub-terms L1 (mul-term-by-all-terms (make-term new-o new-c) L2))
		      L2)))
		(list (addjoin-term (make-term new-o new-c)
				    (car rest-of-result))
		      (cadr rest-of-result))))))))

;; the remainder of result of div-terms should be deprecated in div-poly
(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (let ((t1 (term-list p1))
	    (t2 (term-list p2)))
	(let ((result (div-terms t1 t2)))
	  (make-poly (variable p1)
		     (car result))))
      (error "Polys not in same var -- DIV-POLY"
	     (list p1 p2))))
