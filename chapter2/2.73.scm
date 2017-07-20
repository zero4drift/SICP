(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; a

;; distribution directed by data
;; number and symbol are shceme's base elements, there is no special indicator inside them

;; b & c

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (addend s) (car s))

(define (augend s)
  (if (null? (cdr s))
      0
      (cons '+ (cdr s))))

(define (multiplier p) (car p))

(define (multiplicand p)
  (if (null? (cdr p))
      1
      (cons '* (cdr p))))

(define (exponent x)
  (cadr x))

(define (base x)
  (car x))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1
	 ((=number? exponent 1) base)
	 ((=number? base 1) 1)
	 (else (list '** base exponent)))))


(define (deriv-product . operands var)
  (make-sum
   (make-product (multiplier operands)
		 (deriv (multiplicand operands) var))
   (make-product (deriv (multiplier operands) var)
		 (multiplicand operands))))

(define (deriv-sum . operands var)
  (make-sum (deriv (addend operands) var)
	    (deriv (augend operands) var)))

(define (deriv-exponentiation . operands var)
  (make-product (exponent operands)
		(make-product
		 (make-exponentiation
		  (base operands)
		  (make-sum (exponent operands) (- 1)))
		 (deriv (base operands) var))))

;; install

(define (install-deriv-package)
  (put 'deriv '* deriv-product)
  (put 'deriv '+ deriv-sum)
  (put 'deriv '** deriv-exponentiation))

;; d

(define (install-deriv-package)
  (put '* 'deriv deriv-product)
  (put '+ 'deriv deriv-sum)
  (put '** 'deriv deriv-exponentiation))
