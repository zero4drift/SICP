(define (make-frame bindings)
  bindings)

(define (make-binding var val)
  (cons var val))

(define (make-bindings vars vals)
  (map make-binding vars vals))

(define (bindings-frame frame)
  frame)

(define (set-value! binding value)
  (set-cdr! binding value))

(define (first-binding bindings)
  (car bindings))

(define (rest-bindings bindings)
  (cdr bindings))

(define (null-bindings? bindings)
  (null? bindings))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (add-binding-to-frame! var val frame)
  (let ((new-binding (make-binding val frame)))
    (cons new-binding frame)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (let ((bindings (make-bindings vars vals)))
	(cons (make-frame bindings) base-env))
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (lookup var bindings)
    (if (null-bindings? bindings)
	false
	(let ((first (first-binding bindings)))
	  (if (eq? var (binding-variable first))
	      (binding-value first)
	      (lookup var (rest-bindings bindings))))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env))
	    (base-env (enclosing-environment env)))
	(let ((result (lookup var (bindings-frame frame))))
	  (if result
	      result
	      (lookup-variable-value var base-env))))))

(define (set-variable-value! var val env) ;the former one has too many lets, try another style
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null-bindings? bindings)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (binding-variable (first-binding bindings)))
	     (set-value! (first-binding bindings) val))
	    (else (scan (rest-bindings bindings)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (bindings-frame frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null-bindings? bindings)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (binding-variable (first-binding bindings)))
	     (set-value! (first-binding bindings) val))
	    (else (scan (rest-bindings bindings)))))
    (scan (bindings-frame frame))))
