;; requiere the memq procedure of exercise 3.17

;; check whether the element self-contains

(define (infinite? l)
  (let ((checked '()))
    (define (recursive x)
      (cond ((null? x) false)
	    ((memq x checked) true)
	    (else
	     (begin (set! checked (cons x checked))
		    (recursive (cdr x))))))
    (recursive l)))
