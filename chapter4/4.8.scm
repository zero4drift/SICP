(define (let? exp)
  (tagged-list? exp 'let))

(define (let-exps exp)
  (cdr exp))

(define (let-procedure? exp)
  (symbol? (car exp)))

(define (let-procedure-var exp)
  (car exp))

(define (let-procedure-bindings exp)
  (cadr exp))

(define (let-procedure-body exp)
  (cddr exp))

(define (make-begin seq)
  (cons 'begin seq))

(define (make-define-p v p b)
  (cons 'define (cons v p) b))

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

(define (transfer exp)
  (let ((let-p? (let-procedure? exp)))
    (let ((bindings
	   (if let-p?
	       (let-procedure-bindings exp)
	       (let-bindings exp)))
	  (body
	   (if let-p?
	       (let-procedure-body exp)
	       (let-body exp))))
      (let ((var-list
	     (map var bindings))
	    (exp-list
	     (map expression bindings)))
	(if let-p?
	    (make-begin
	     (list (make-define-p
	       (let-procedure-var exp)
	       var-list
	       body)
	      (cons (let-procedure-var exp)
		    exp-list)))
	    (cons (make-lambda var-list body)
		  exp-list))))))

(define (eval-let exp env)
  (eval (let->application exp) env))

;; implement let? and eval-let in eval
