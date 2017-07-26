;; requiere the memq procedure of exercise 3.17.scm

(define (list-walk steps list)
  (cond ((null? list) '())
	((= steps 0) list)
	(else
	 (list-walk
	  (- steps 1)
	  (cdr list)))))

(define (clever-infinite? l)
  (define (iter x y)
    (let ((a (list-walk 1 x))
	  (b (list-walk 2 y)))
      (cond ((or (null? a) (null? b)) false)
	    ((eq? a b) true)
	    (else
	     (iter a b)))))
  (iter l l))
