;; install project
(define (project-rational x)
  (make-scheme-number (round (/ (numer x) (denom x)))))
(define (project-real x)
  (make-scheme-number (round x)))
;; notice that we have not implement real package
;; assume that it was implemented
(define (project-complex x)
  (make-real (real-part x)))

(define (install-project)
  (put 'project '(rational) project-rational)
  (put 'project '(real) project-real)
  (put 'project '(complex) project-complex)
  'done)

(define (projectt x)
  (apply-generic 'project x))

;; install drop
(define (drop-scheme-number x)
  x)

(define (drop-rational x)
  (let ((a (project x)))
    (if (equ? (raise a) x)
	a
	x)))

(define (drop-real x)
  (let ((a (project x)))
    (if (equ? (raise (raise a)) x)
	a
	x)))

(define (drop-complex x)
  (let ((a (project x)))
    (if (equ? (raise a) x)
	(drop a)
	x)))	

(define (install-drop)
  (put 'drop '(scheme-number) drop-scheme-number)
  (put 'drop '(rational) drop-rational)
  (put 'drop '(real) drop-real)
  (put 'drop '(complex) drop-complex)
  'done)

(define (drop n)
  (apply-generic 'drop n))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc	      
	  (let ((res (apply proc (map contents args))))
	    (if (or (eq? op 'raise) (eq? op 'equ?) (eq? op 'drop))
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
