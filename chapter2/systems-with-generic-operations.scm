(load "../chapter3/3.24.scm")

;; make a type-operation table
(define tb (make-table equal?))
(define get (tb 'lookup-proc))
(define put (tb 'insert-proc!))
;; table done

;; overall general procedures

;; follow ex 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else
	 (error "Bad tagged datum -- CONTENTS" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(else
	 (error "Bad tagged datum -- CONTENTS" datum))))
;; 2.78

;; follow 2.81
;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;; 	  (apply proc (map contents args))
;; 	  (if (= (length args) 2)
;; 	      (let ((type1 (car type-tags))
;; 		    (type2 (cadr type-tags))
;; 		    (a1 (car args))
;; 		    (a2 (cadr args)))
;; 		(if(equal? type1 type2)
;; 		   (error "No method for these types"
;; 			  (list op type-tags))
;; 		   (let ((t1->t2 (get-coercion type1 type2))
;; 			 (t2->t1 (get-coercion type2 type1)))
;; 		     (cond (t1->t2
;; 			    (apply-generic op (t1->t2 a1) a2))
;; 			   (t2->t1
;; 			    (apply-generic op a1 (t2->t1 a2)))
;; 			   (else
;; 			    (error "No method for these types"
;; 				   (list op type-tags))))))
;; 		(error "No method for these types"
;; 		       (list op type-tags))))))))
;; 2.81


;; follow ex 2.84
;; types-tower
(define (install-types-tower)
  (put 'level 'scheme-number 1)
  (put 'level 'rational 2)
  ;; real package is not implemented
  (put 'level 'real 3)
  (put 'level 'complex 4)
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  ;; follow 2.85
	  ;; (let ((res (apply proc (map contents args))))
	  ;;   (if (or (eq? op 'raise) (eq? op 'equ?) (eq? op '=zero?))
	  ;; 	res
	  ;; 	(drop res)))
	  ;; 2.85
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(cond ((equal? type1 type2)
		       (error "No method for these types"
			      (list op type-tags)))
		      ((< (level a1) (level a2))
		       (apply-generic op (raise a1) a2))
		      (else (apply-generic op a1 (raise a2)))))
	      (error "No method for these types"
		     (list op type-tags)))))))
;; 2.84

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
;; follow ex 2.79
(define (equ? x y)(apply-generic 'equ? x y))
;; 2.79
;; follow ex 2.80
(define (=zero? x) (apply-generic '=zero? x))
;; 2.80
;; follow 2.83
(define (raise number) (apply-generic 'raise number))
;; 2.83
;; follow 2.84
(define (level x) (get 'level (type-tag x)))
;; 2.84
;; follow ex 2.85
(define (project x) (apply-generic 'project x))
(define (drop x)
  (if (number? x)
      x
      (let ((a (project x)))
	(if (equ? (raise a) x)
	    (drop a)
	    x))))
;; 2.85
;; follow ex 2.86
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x) (apply-generic 'arctan x))
(define (exp x y) (apply-generic 'exp x y))
;; 2.86
;; follow ex 2.87
(define (negative n)
  (apply-generic 'negative n))
;; 2.87
;; follow ex 2.93 & 2.94
(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b))
;; 2.93 & 2.94
;; follow ex 2.97
(define (reduce n d) (apply-generic 'reduce n d))
;; 2.97


;; helper functions
(define (square x) (mul x x))
(define (sqr x) (exp x 0.5))
;; built-in gcd


;; scheme-number package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  ;; follow ex 2.79
  (define (equ-number? n1 n2)
    (= n1 n2))
  (put 'equ? '(scheme-number scheme-number) equ-number?)
  ;; 2,79
  ;; follow ex 2.80
  (define (=zero-number? n) (= n 0))
  (put '=zero? '(scheme-number) =zero-number?)
  ;; 2.80
  ;; follow 2.83
  (define (scheme-number->rational number)
    (make-rational number 1))
  (put 'raise '(scheme-number) scheme-number->rational)
  ;; 2.83
  ;; follow 2.85
  (define (drop-scheme-number x) x)
  (put 'drop '(scheme-number) drop-scheme-number)
  ;; 2.85
  ;; follow ex 2.93 & 2.94
  (define gcd-scheme-number gcd)
  (put 'gcd '(scheme-number scheme-number)
       (lambda (s1 s2) (tag (gcd-scheme-number s1 s2))))
  ;; 2.93 & 2.94
  ;; follow ex 2.97
  (define (reduce-integers n d)
    (let ((g (greatest-common-divisor n d)))
      (list (div n g) (div d g))))
  ;; 2.97
  (put 'reduce '(scheme-number scheme-number)
       (lambda (n d) (map tag (reduce-integers n d))))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  ;; follow ex 2.86
  (put 'sine '(scheme-number) (lambda (x) (tag (sin x))))
  (put 'cosine '(scheme-number) (lambda (x) (tag (cos x))))
  (put 'arctan '(scheme-number scheme-number) (lambda (y x) (tag (atan y x))))
  (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
  ;; 2.86
  ;; follow 2.88
  (define (negative-scheme-number n)
    (make-scheme-number (- n)))
  (put 'negative '(scheme-number) negative-scheme-number)
  ;; 2.88

  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ;; (define (make-rat n d)
  ;;   ;; follow ex 2.93 & 2.94
  ;;   (let ((g (greatest-common-divisor n d)))
  ;;     (cons (div n g) (div d g))))
  ;; follow ex 2.97
  (define (make-rat n d)
    (let ((simple (reduce n d)))
      (cons (car simple) (cadr simple))))
  ;; 2.97
  ;; 2.93 & 2.94
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; follow ex 2.79
  (define (equ-rational? r1 r2)
    (let ((product1 (* (numer r1) (denom r2)))
	  (product2 (* (denom r1) (numer r2))))
      (= product1 product2)))
  (put 'equ? '(rational rational) equ-rational?)
  ;; 2.79
  ;; follow ex 2.80
  (define (=zero-rational? r)
    (= (numer r) 0))
  (put '=zero? '(rational) =zero-rational?)
  ;; 2.80
  ;; follow 2.83
  ;; note that we do not implement a real package
  ;; (define (rational->real number)
  ;;   (make-real (/ (numer number) (denom number))))
  ;; (put 'raise '(rational) rational->real)
  ;; 2.83
  ;; follow 2.85
  (define (project-rational x)
    (make-scheme-number (round (/ (numer x) (denom x)))))
  (put 'project '(rational) project-rational)
  (define (drop-rational x)
    (let ((a (project x)))
      (if (equ? (raise a) x)
	  a
	  x)))
  (put 'drop '(rational) drop-rational)
  ;; 2.85
  ;; follow 2.88
  (define (negative-rational n)
    (make-rational (negative (numer n)) (denom n)))
  (put 'negative '(rational) negative-rational)
  ;; 2.88
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ;; follow ex 2.86
  ;; assume that there is a way of computing numer and denom from real number(*.*)
  ;; calls get_numer_denom, which returns a numer-denom pair
  ;; Alternative:
  ;; if not strictly stricted, sine/cosine/arctan/exp of rational could return scheme number;
  ;; just replace (tag (get_numer_denom)) with (make-scheme-number)
  ;; (put 'sine '(rational) (lambda (x) (tag (get_numer_denom (sine (/ (numer x) (denom x)))))))
  ;; (put 'cosine '(rational) (lambda (x) (tag (get_numer_denom (cosine (/  (numer x) (denom x)))))))
  ;; (put 'arctan '(rational rational)
  ;;      (lambda (y x) (tag (get_numer_denom
  ;; 		      (arctan (/ (numer y) (denom x)) (/ (numer x) (denom x)))))))
  ;; (put 'exp '(rational rational)
  ;;      (lambda (x y) (tag (get_numer_denom
  ;; 		      (exp (/ (numer x) (denom x)) (/ (numer y) (denom y)))))))
  ;; 2.86
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
		       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
		       (sub (angle z1) (angle z2))))
  ;; follow ex 2.79
  (define (equ-complex? c1 c2)
    (let ((real1 (real-part c1))
	  (imag1 (imag-part c1))
	  (real2 (real-part c2))
	  (imag2 (imag-part c2)))
      (and (= real1 real2) (= imag1 imag2))))
  (put 'equ? '(complex complex) equ-complex?)
  ;; 2.79
  ;; follow ex 2.80
  (define (=zero-complex? c)
    (let ((real (real-part c))
	  (imag (imag-part c)))
      (and (= real 0) (= imag 0))))
  (put '=zero? '(complex) =zero-complex?)
  ;; 2.80
  ;; follow ex 2.85
  ;; need real package
  ;; (define (project-complex x) (make-real (real-part x)))
  ;; (put 'project '(complex) project-complex)
  ;; (define (drop-complex x)
  ;;   (let ((a (project x)))
  ;;     (if (equ? (raise a) x)
  ;; 	  (drop a)
  ;; 	  x)))	
  ;; (put 'drop '(complex) drop-complex)
  ;; 2.85
  ;; follow ex 2.88
  (define (negative-complex n)
    (make-complex-from-real-imag (negative (real-part n)) (negative (imag-part n))))
  (put 'negative '(complex) negative-complex)
  ;; 2.88

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))  
  'done)

;; rectangular-package for complex package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqr (add (square (real-part z))
	      (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; polar-package for complex package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z)) 
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqr (add (square x) (square y)))
          (arctan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; follow ex 2.89 & 2.90
;; install common polynomial term package
(define (install-poly-term-package)
  ;; procedures kept same
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;; tag
  (define (tag term)
    (attach-tag 'polynomial-term term))
  ;; put
  (put 'make 'polynomial-term
       (lambda (x y) (tag (make-term x y))))
  (put 'order '(polynomial-term) order)
  (put 'coeff '(polynomial-term) coeff)
  'done)
(define (make-term x y)
  ((get 'make 'polynomial-term) x y))
(define (order x)
  (apply-generic 'order x))
(define (coeff x)
  (apply-generic 'coeff x))

(define (install-dense-terms-package)
  ;; procedures about dense terms...
  (define (tag terms)
    (attach-tag 'polynomial-dense terms))
  (define (first-term-d term-list)
    (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms-d term-list) (cdr term-list))
  (define (empty-termlist-d? term-list) (null? term-list))
  (define (adjoin-term-d term term-list)
    (cond ((=zero? (coeff term)) term-list)
	  ((equ? (order term) (length term-list))
	   (cons (coeff term) term-list))
	  ((> (order term) (length term-list))
	   (adjoin-term-d term (cons 0 term-list))))) ;
  (define (negative-d terms)
    (if (empty-termlist-d? terms)
	terms
	(let ((first (car terms)))
	  (cons (negative first)
		(negative-d (rest-terms-d terms))))))
  ;; follow ex 2.96
  (define (coeffs-d terms)
    (cond ((empty-termlist-d? terms) '())
	  ((equ? (car terms) 0) (coeffs-d (cdr terms)))
	  (else (cons (car terms) (coeffs-d (cdr terms))))))
  (put 'coeffs '(polynomial-dense) coeffs-d)
  ;; 2.96
  (put 'negative '(polynomial-dense)
       (lambda (x) (tag (negative-d x))))
  (put 'first-term '(polynomial-dense)
       (lambda (x) (first-term-d x)))
  (put 'empty-termlist? '(polynomial-dense) empty-termlist-d?)
  (put 'rest-terms '(polynomial-dense)
       (lambda (x) (tag (rest-terms-d x))))
  (put 'adjoin-term 'polynomial-dense
       (lambda (x y) (tag (adjoin-term-d x y))))
  (put 'make 'polynomial-dense (lambda (t) (tag t)))
  'done)

(define (make-dense-terms terms)
  ((get 'make 'polynomial-dense) terms))

(define (install-sparse-terms-package)
  ;; procedures about sparse terms...
  (define (tag terms)
    (attach-tag 'polynomial-sparse terms))
  (define (first-term-s term-list) (car term-list))
  (define (adjoin-term-s term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (rest-terms-s term-list) (cdr term-list))
  (define (empty-termlist-s? term-list) (null? term-list))
  (define (negative-s terms)
    (if (empty-termlist-s? terms)
	terms
	(let ((first (first-term-s terms)))
	  (adjoin-term-s (make-term
			  (order first)
			  (negative (coeff first)))
			 (negative-s (rest-terms-s terms))))))
  ;; follow ex 2.96
  (define (coeffs-s terms)
    (map (lambda (t) (coeff t)) terms))
  (put 'coeffs '(polynomial-sparse) coeffs-s)
  ;; 2.96
  (put 'negative '(polynomial-sparse)
       (lambda (x) (tag (negative-s x))))
  (put 'first-term '(polynomial-sparse)
       (lambda (x) (first-term-s x)))
  (put 'empty-termlist? '(polynomial-sparse) empty-termlist-s?)
  (put 'rest-terms '(polynomial-sparse)
       (lambda (x) (tag (rest-terms-s x))))
  (put 'adjoin-term 'polynomial-sparse
       (lambda (x y) (tag (adjoin-term-s x y))))
  (put 'make 'polynomial-sparse (lambda (t) (tag t)))
  'done)
(define (make-sparse-terms terms)
  ((get 'make 'polynomial-sparse) terms))

(define (first-term x)
  (apply-generic 'first-term x))

(define (adjoin-term x y)
  ((get 'adjoin-term (type-tag y)) x (contents y)))

(define (negative x)
  (apply-generic 'negative x))

(define (empty-termlist? x)
  (apply-generic 'empty-termlist? x))

(define (rest-terms x)
  (apply-generic 'rest-terms x))

(define (make-terms t terms)
  ((get 'make t)
   (if (equal? t 'polynomial-sparse)
       terms
       (map coeff terms))))
;; 2.89 & 2.90
;; follow 2.96
(define (coeffs t)
  (apply-generic 'coeffs t))
;; 2.96

;; polynomial-package
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; terms
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
		    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else
		    (adjoin-term
		     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (add (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  ;; follow ex 2.91
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
	(list L1 L1)
	(let ((t1 (first-term L1))
	      (t2 (first-term L2)))
	  (if (> (order t2) (order t1))
	      (list (make-terms (type-tag L1) '()) L1)
	      (let ((new-c (div (coeff t1) (coeff t2)))
		    (new-o (sub (order t1) (order t2))))
		(let ((rest-of-result
		       (div-terms
			(add-terms L1
				   (negative (mul-term-by-all-terms
					      (make-term new-o new-c)
					      L2)))
			L2)))
		  (list (adjoin-term (make-term new-o new-c)
				     (car rest-of-result))
			(cadr rest-of-result))))))))
  ;; follow ex 2.96
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
  ;; 2.96

  ;; follow 2.97
  (define (reduce-terms n d)
    (define (compute-constant a b c)
      (let ((af (first-term a))
	    (bf (first-term b))
	    (cf (first-term c)))
	(let ((c (coeff af))
	      (o1 (max (order bf) (order cf)))
	      (o2 (order af)))
	  (exp c (+ 1 o1 (- o2))))))
    (define (simplify a b)
      (let ((g (gcd-terms-coeff (append (coeffs a) (coeffs b)))))
	(let ((divider (make-terms (type-tag a) (list (make-term 0 g)))))
	  (list (car (div-terms a divider)) (car (div-terms b divider))))))
    (let ((g (gcd-terms n d)))
      (let ((constant (compute-constant g n d)))
	(let ((mn (mul-term-by-all-terms (make-term 0 constant) n))
	      (md (mul-term-by-all-terms (make-term 0 constant) d)))
	  (let ((gn (car (div-terms mn g)))
		(gd (car (div-terms md g))))
	    (simplify gn gd))))))	
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(let ((t1 (term-list p1))
	      (t2 (term-list p2)))
	  (let ((result (reduce-terms t1 t2)))
	    (list (make-poly (variable p1) (car result))
		  (make-poly (variable p1) (cadr result)))))
	(error "Polys not in same variable -- REDUCE-POLY"
	       (list p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (n d) (map tag (reduce-poly n d))))
  ;; 2.97

  ;; 2.91
  ;; follow ex 2.87
  (define (=zero?-p p)
    (define (recursive terms)
      (if (empty-termlist? terms)
	  #t
	  (let ((first (first-term terms))
		(rest (rest-terms terms)))
	    (if (=zero? (coeff first))
		(recursive rest)
		#f))))
    (recursive (term-list p)))
  (put '=zero? '(polynomial) =zero?-p)
  ;; 2.87

  ;; poly
  ;; follow 2.88
  (define (negative-poly n)
    (make-poly (variable n) (negative (term-list n))))
  (put 'negative '(polynomial)
       (lambda (x) (tag (negative-poly x))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negative-poly p2)))
  (put 'sub '(polynomial polynomial)
       (lambda (x y) (tag (sub-poly x y))))
  ;; 2.88
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
                   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- ADD-POLY"
	       (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
                   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))
  ;; follow 2.91
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(let ((t1 (term-list p1))
	      (t2 (term-list p2)))
	  (let ((result (div-terms t1 t2)))
	    (make-poly (variable p1)
		       (car result))))
	(error "Polys not in same var -- DIV-POLY"
	       (list p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  ;; 2.91
  ;; follow 2.93 & 2.94
  (define (gcd-poly a b)
    
    ;; (define (remainder-terms t1 t2)
    ;;   (cadr (div-terms t1 t2)))
    ;; (define (gcd-terms t1 t2)
    ;;   (if (empty-termlist? t2)
    ;; 	  t1
    ;; 	  (gcd-terms t2 (remainder-terms t1 t2))))
    
    (if (same-variable? (variable a) (variable b))
	(let ((t1 (term-list a))
	      (t2 (term-list b)))
	  (make-poly (variable a) (gcd-terms t1 t2)))
	(error "Polys not in same var -- GCD-POLY"
	       (list a b))))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  ;; 2.93 & 2.94

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


;; install
(install-types-tower)
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-poly-term-package)
(install-dense-terms-package)
(install-sparse-terms-package)
(install-polynomial-package)
