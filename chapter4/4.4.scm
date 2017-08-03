(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (all-predicates exp)
  (cdr exp))

(define (first-predicate predicates)
  (car predicates))

(define (rest-predicates predicates)
  (cdr predicates))

(define (eval-and exp env)
  (let ((predicates (all-predicates exp)))
    (let ((first (first-predicate predicates))
	  (rest (rest-predicates predicates)))
      (let ((result (eval first env)))
	(cond ((not result) false)
	      ((and (null? rest) result) result)
	      (result (eval-and rest env)))))))

(define (eval-or exp env)
  (let ((predicates (all-predicates exp)))
    (let ((first (first-predicate predicates))
	  (rest (rest-predicates predicates)))
      (let ((result (eval first env)))
	(cond (result true)
	      (and (null? rest) (not result) false)
	      ((not result) eval-or rest env))))))

;; derived expression

(define (make-and predicates)
  (list 'and predicates))

(define (make-or predicates)
  (list 'or predicates))

(define (make-or predicates)
  (list 'or predicates))

(define (make-cond clause1 clause2 clause3)
  (list 'cond clause1 clause2 clause3))

(define (and->cond exp)
  (let ((predicates (all-predicates exp)))
    (let ((first (first-predicate predicates))
	  (rest (rest-predicates predicates)))
      (make-cond
       ((false? first) false)
       ((and (null? rest) (true? first)) first)
       (else (make-and rest))))))

(define (or->cond exp)
  (let ((predicates (all-predicates exp)))
    (let ((first (first-predicate predicates))
	  (rest (rest-predicates predicates)))
      (make-cond
       ((true? first) true)
       ((and (null? rest) (false? first)) false)
       (else (make-or rest))))))

(define (eval-and exp env)
  (eval (and->cond exp) env))

(define (eval-or exp env)
  (eval (or->cond exp) env))

;; so just implement the eval-and and eval-or in
;; the original eval procedure
