(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

;; reverse the second list parameter and return the result
;; v: (list 'a 'b 'c 'd)
;; w: (list 'd 'c 'b 'a)
;; v: (list 'a)
