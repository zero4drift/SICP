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

;; derived expression based on if

(define (make-and predicates)
  (list 'and predicates))

(define (make-or predicates)
  (list 'or predicates))

(define (and->if exp)
  (let ((predicates (all-predicates exp)))
    (let ((first (first-predicate predicates))
	  (rest (rest-predicates predicates)))
      (make-if
       first
       (make-and rest)
       first))))

(define (or->cond exp)
  (let ((predicates (all-predicates exp)))
    (let ((first (first-predicate predicates))
	  (rest (rest-predicates predicates)))
      (make-if
       first
       first
       (make-or rest)))))

(define (eval-and exp env)
  (eval (and->if exp) env))

(define (eval-or exp env)
  (eval (or->if exp) env))

;; so just implement the eval-and and eval-or in
;; the original eval procedure
