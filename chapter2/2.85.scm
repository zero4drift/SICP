(define (drop-scheme-number x)
  (x))

(define (drop-rational-number x)
  (define (project x)
    (make-scheme-number (round (/ (numer x) (denom x)))))
  (let ((a (project x)))
    (if (equ? (raise a) x)
	(drop a)
	x)))

(define (drop-real-number x)
  (define (project x)
    (make-scheme-number (round x)))
  (let ((a (project x)))
    (if (equ? (raise (raise a)) x)
	(drop a)
	x)))

(define (drop-complex-number x)
  (define (project x)
    (make-real (real-part x)))
  (let ((a (project x)))
    (if (equ? (raise a) x)
	(drop a)
	x)))				;most details of these four drop-xxx procedures could be implemented in drop procedur

(define (install-drop)
  (put 'drop 'scheme-number drop-scheme-number)
  (put 'drop 'rational drop-rational-number)
  (put 'drop 'real drop-real-number)
  (put 'drop 'complex drop-complex-number)
  'done)

(define (drop n)
  (let ((tag (type-tag n)))
    (apply-generic 'drop n)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc	      
	  (let ((res (apply proc (map contents args))))
	    (if (or (eq? op 'raise) (eq? op 'equ?))
		res
		(drop res)))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(cond ((equal? type1 type2)
		       (error "No method for these types"
			      (list op type-tags)))
		      ((lower-one t1 t2)
		       (apply-generic op (raise a1) a2))
		      (else (apply-generic op a1 (raise a2)))))
	      (error "No method for these types"
		     (list op type-tags)))))))
