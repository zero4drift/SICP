;; a

(define (letrec? exp)
  (tagged-list? exp 'letrec))

;; based on exercise 4.16
;; transfer it to the let with 'unassigned & set!

(define (letrec-exps exp)
  (cdr exp))

(define (letrec-bindings exp)
  (car exp))

(define (letrec-body exp)
  (cdr exp))

(define (var binding)
  (car binding))

(define (expression binding)
  (cadr binding))

(define (letrec->let exp)
  (transfer (letrec-exps exp)))

(define (transfer exp)
  (let ((bindings (letrec-bindings exp))
	(body (letrec-body exp)))
    (let ((var-list (map var bindings))
	  (exp-list (map expression bindings)))
      (append (list 'let (map (lambda (var) (list var 'unassigned)) var-list))
	      (append  (map
			(lambda (var expression) (list 'set! var expression))
			var-list exp-list)
		       body)))))

(define (eval-letrec exp env)
  (eval (letrec->let) env))

;; implement letrec? and eval-letrec in eval


;; b

;; The lambda in `let' is evaluated in the context of the enclosing environment, 
;; in which the bindings of the lambda itself are not in place. 
;; save the illustrations.
