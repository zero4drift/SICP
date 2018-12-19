;; (load "./2.18.scm")

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe tree)
  (define (iter tree result)
    (cond ((null? tree) result)
	  ((pair? (car tree)) (iter (cdr tree) (append (fringe (car tree)) result)))
	  (else (iter (cdr tree) (cons (car tree) result)))))
  (reverse (iter tree '())))
