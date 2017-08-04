(define (scan vars vals var)
  (cond ((null? vars) false)
	((eq? (car vars) var)
	 vals)
	(else (scan (cdr vars) (cdr vals) var))))

(define (env-loop env proc)
  (if (eq? env the-empty-environment)
      false
      (let ((frame (first-frame env)))
	(let ((result (scan (frame-variables frame)
			    (frame-values frame)
			    var)))
	  (if result
	      (proc result)
	      (env-loop (enclosing-environment env)))))))

(define (lookup-variable-value var env)
  (let ((result (env-loop env car)))
    (if result
	result
	(error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (define (set-value! vals)
    (set-car! vals val))
  (let ((result (env-loop env set-value!)))
    (if (not result)			;though set-car! returns unspecific return value, (not result) is false
	(error "Unbound variable" var))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((result (scan (frame-variables frame)
			(frame-values frame)
			var)))
      (if result
	  (set-car! result val)
	  (add-binding-to-frame! var val frame)))))
