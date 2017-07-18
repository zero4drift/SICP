(define (equal? items1 items2)
  (cond ((and (pair? items1) (pair? items2))
	 (and
	  (equal? (car items1) (car items2))
	  (equal? (cdr items1) (cdr items2))))
	((and (symbol? items1) (symbol? items2))
	 (eq? items1 items2))
	((and (null? items1) (null? items2)) true)
	(else false)))
