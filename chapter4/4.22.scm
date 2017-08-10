(define (let? exp)
  (tagged-list? exp 'let))

(define (let-exps exp)
  (cdr exp))

(define (let-bindings exp)
  (car exp))

(define (let-body exp)
  (cdr exp))

(define (var binding)
  (car binding))

(define (expression binding)
  (cadr binding))

(define (let->application exp)
  (transfer (let-exps exp)))

(define (analyze-let exp)
  (let ((bindings (let-bindings exp))
	(body (let-body exp)))
    (let ((var-list (map var bindings))
	  (exp-list (map expression bindings)))
      (let ((proc (make-lambda var-list body)))
	(analyze-application
	 (cons proc exp-list))))))
