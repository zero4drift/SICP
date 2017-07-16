(define (fringe tree)
  (define (iter tree result)
    (cond ((null? tree) result)
	  ((pair? (car tree)) (iter (cdr tree) (append (fringe (car tree)) result)))
	  (else (iter (cdr tree) (cons (car tree) result)))))
  (reverse (iter tree '())))
