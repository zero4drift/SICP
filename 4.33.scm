(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (let ((temp (cadr exp)))
    (if (not (pair? temp))
	temp
	(new-list temp))))

(define (new-list pair)
  (if (null? pair)
      '()
      (make-procedure
       '(m)
       (list (list 'm 'car-value 'cdr-value))
       (extend-environment
	(list 'car-value 'cdr-value)
	(list (car pair) (new-list (cdr pair)))
	the-empty-environment))))
