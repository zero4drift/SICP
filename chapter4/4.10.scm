;; just like exercise 4.1
;; change the evaluation order of application parameters

;; change the position of the operator
;; replace it at the end application

(define (last-one lst)
  (if (null? (cdr lst))
      (car lst)
      (last-one (cdr lst))))

(define (operator exp)
  (last-one exp))

(define (operands exp)
  (if (null? (cdr exp))
      '()
      (cons (car exp)
	    (operands (cdr exp)))))
