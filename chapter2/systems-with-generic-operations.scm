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
	  (let ((res (apply proc (map contents args))))
	    (if (or (eq? op 'raise) (eq? op 'equ?) (eq? op '=zero?))
	  	res
	  	(drop res)))
	  ;; 2.85
	  ;; (apply proc (map contents args))
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
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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


;; install
(install-types-tower)
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
