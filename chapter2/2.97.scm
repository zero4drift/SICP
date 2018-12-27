;; (load "./2.96.scm")
;; a

(define (reduce-terms n d)
  (define (compute-constant a b c)
    (let ((af (first-term a))
	  (bf (first-term b))
	  (cf (first-term c)))
      (let ((c (coeff af))
	    (o1 (max (order bf) (order cf)))
	    (o2 (order af)))
	(expt c (+ 1 o1 (- o2))))))
  (define (simplify a b)
    (let ((g (gcd-terms-coeff (append a b))))
      (let ((sa (car (div-terms a (list (make-term 0 g)))))
	    (sb (car (div-terms b (list (make-term 0 g))))))
	(list sa sb))))
  (let ((g (gcd-terms n d)))
    (let ((constant (compute-constant g n d)))
      (let ((mn (mul-term-by-all-terms (make-term 0 constant) n))
	    (md (mul-term-by-all-terms (make-term 0 constant) d)))
	(let ((gn (car (div-terms mn g)))
	      (gd (car (div-terms md g))))
	  (simplify gn gd))))))	

(define (reduce-poly p1 p2)
  (if (same-vaiable? (variable p1) (variable p2))
      (let ((t1 (term-list p1))
	    (t2 (term-list p2)))
	(let ((result (reduce-terms t1 t2)))
	  (list (make-poly (variable p1) (car result))
		(make-poly (variable p1) (cadr result)))))
      (error "Polys not in same variable -- REDUCE-POLY"
	     (list p1 p2))))

;; b

;; inside procedure install-scheme-number-package
(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))
(put 'reduce '(scheme-number scheme-number)
     (lambda (n d) (map tag (reduce-integers n d))))

;; inside procedure install-polynomial-package
(put 'reduce '(polynomial polynomial)
     (lambda (n d) (map tag (reduce-poly n d))))

;; inside procedure install-rational-package
(define (make-rat n d)
  (let ((simple (reduce n d)))
    (cons (car simple) (cadr simple))))

(define (reduce n d)
  (apply-generic 'reduce n d))
