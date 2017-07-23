;; do not make it too complex

(define (the-empty-termlist) '())

(define (empty-termlist? term-list)
  (null? term-list))

(define (first-term term-list)
  (car term-list))

(define (rest-terms term-list)
  (cdr term-list))

(define (make-term term)
  term)

(define (adjoin-term term term-list)
  (cons term term-list))
  
;; procedures order and coeff are not necessary here

(define (add-terms l1 l2)
  (cond ((empty-termlist? l1) l2)
	((empty-termlist? l2) l1)
	(else
	 (let ((t1 (first-term l1)) (t2 (first-term l2)))
	   (adjoin-term
	    (make-term (add t1 t2))
	    (add-terms (rest-terms l1)
		       (rest-terms l2)))))))

(define (mul-terms l1 l2)
  (cons ((empty-termlist? l1)
	 (the-empty-termlist))
	(add-terms (mul-term-by-all-terms (first-term l1) l2)
		   (mul-terms (rest-terms l1) l2))))

(define (mul-term-by-all-terms term l)
  (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t (first-term l)))
	(adjoin-term
	 (make-term (mul (term t)))
	 (mul-term-by-all-terms term (rest-terms l))))))
