;; all bacause of the environment bindings' update & access

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*-exps exp)
  (cdr exp))

(define (let*-bindings exp)
  (car exp))

(define (let*-body exp)
  (cddr exp))

(define (make-let binding body)
  (cons 'let (list binding) body))

(define (make-neseted-lets bindings body)
  (if (null? (cdr bindings))
      (make-let (car bindings) body)
      (make-let (car bindings)
		(make-neseted-lets
		 (cdr bindings)
		 body))))

(define (let*->nested-lets exp)
  (make-neseted-lets
   (let*-bindings (let*-exps exp))
   (let*-body (let*-exps exp))))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))
;; nested-let makes no difference when compared with let*
;; thanks to the environment structure

;; just implement let*? and eval-let* in eval
