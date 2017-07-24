(define (install-variable-priority)
  (put 'a 'priority 1)
  ;; ...
  (put 'z 'priority 26))

(define (make-poly-tag variable term-list)
  ((get 'make 'polynomial) variable term-list))

(define (transform poly var)
  (let ((old-var (variable poly))
	(old-terms (term-list poly)))
    (define (transform-terms terms)
      (if (null? terms)
	  (the-empty-termlist)
	  (add-terms
	   (transform-term (first-term terms))
	   (transform-terms (rest-terms terms)))))
    (define (transform-term term)
      (let ((c (coeff term))
	    (o (order term)))
	(if (equal? 'polynomial (type-tag c))
	    (mul-terms
	     (list (make-term
		    0
		    (make-poly-tag old-var
			       (make-term o 1))))
	     (term-list (transform (contents c) var)))
	    (list (make-term
		   0
		   (make-poly-tag
		    old-var
		    (list term)))))))
    (if (equal? old-var var)
	poly
	(make-poly
	 var
	 (transform-terms old-terms)))))

(define (higher-priority x y)
  (if (> (get x 'priority) (get y 'priority))
      x
      y))

(define (add-poly p1 p2)
  (let ((v1 (variable p1)) (v2 (variable p2)))
    (if (same-vaiable? v1 v2)
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(let ((higher-priority-one (higher-priority v1 v2)))
	  (add-poly (transform p1 higher-priority-one)
		    (transform p2 higher-priority-one))))))

(define (mul-poly p1 p2)
  (let ((v1 (variable p1)) (v2 (variable p2)))
    (if (same-vaiable? v1 v2)
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(let ((higher-priority-one (higher-priority v1 v2)))
	  (mul-poly (transform p1 higher-priority-one)
		    (transform p2 higher-priority-one))))))

;; other procedures kept unchanged
