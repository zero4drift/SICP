(define (user-print object)
  (cond ((compound-procedure? object)
	 (display (list 'compound-procedure
			(procedure-parameters object)
			(procedure-body object)
			'<procedure-env>)))
	((lazy-list? object)
	 (display (list 'lazy-list
			(caddddr object)
			'...)))
	(else (display object))))

;; reserve the old cons car cdr in evaluator

(define old-cons cons)

(define old-car car)

(define old-cdr cdr)

(define (tagged-list? exp symbol)
  (eq? (old-car exp) symbol))

(define (lazy-list? exp)
  (tagged-list? exp 'lazy-list))

(define (cons x y)
  (old-cons 'lazy-list (lambda (m) (m x y))))

(define (car z)
  ((old-cdr z) (lambda (p q) p)))

(define (cdr z)
  ((old-cdr z) (lambda (p q) q)))
