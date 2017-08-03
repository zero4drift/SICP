(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-special-clause? clause)
  (eq? '=> (cadr clause)))

(define (cond-special-clause-proc clause)
  (caddr clause))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF"
		       clauses))
	    (let ((p (cond-predicate first)))
	      (make-if
	       p
	       (if (cond-special-clause? first)
		   (list (cond-special-clause-proc first) p)
		   (sequence->exp (cond-actions first)))
	       (expand-clauses rest)))))))
