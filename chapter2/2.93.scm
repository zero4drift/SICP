;; just define procedure gcd-terms is not enough;
;; need a few more definitions and operations:

(define (install-scheme-number-package)
  ;; internal procedures
  ;; other procedures unchanged
  ;; gcd-scheme-number is the original version of gcd
  (put 'gcd '(scheme-number scheme-number)
       (lambda (s1 s2) (tag (gcd-scheme-number s1 s2))))
  )

(define (install-rational-package)
  ;; internal procedures
  ;; just replace every +, - , / and * with add, sub, div and mul
  (define (make-rat n d)
    (let ((g (greatest-common-divisor n d)))
      (cons (div n g) (div d g))))
  ;; other procedures unchanged
  )

(define (install-polynomial-package)
  ;; other internal prodedures kept unchanged
  (define (gcd-poly a b)
    (define (remainder-terms t1 t2)
      (cadr (div-terms t1 t2)))
    (define (gcd-terms t1 t2)
      (if (empty-termlist? t2)
	  t1
	  (gcd-terms t2 (remainder-terms t1 t2))))
    (if (same-variable? (variable a) (variable b))
	(let ((t1 (term-list a))
	      (t2 (term-list b)))
	  (make-poly (variable a) (gcd-terms t1 t2)))
	(error "Polys not in same var -- GCD-POLY"
	       (list a b))))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  )

(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b))
