(define (let? exp)
  (tagged-list? exp 'let))

(define (let-exps exp)
  (cdr exp))

(define (let-bindings exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (var binding)
  (car binding))

(define (expression binding)
  (cadr binding))

(define (let->application exp)
  (transfer (let-exps exp)))

(define (transfer exp)
  (let ((bindings (let-bindings exp))
	(body (let-body exp)))
    (let ((var-list (map var bindings))
	  (exp-list (map expression bindings)))
      (let ((proc (make-lambda var-list body)))
	(list proc exp-list)))))

(define (eval-let exp env)
  (eval (let->application exp) env))

;; implement let? and eval-let in eval
