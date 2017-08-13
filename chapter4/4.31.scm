;; in eval
((application? exp)
 (apply (actual-value (operator exp) env)
	(operands exp)
	env))				;keep same

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure
	  procedure
	  (list-of-arg-values arguments env)))
	((compound-procedure? procedure)
	 (let ((parameters (procedure-parameters procedure)))
	   (eval-sequence
	    (procedure-body procedure)
	    (extend-environment
	     (map (lambda (i) (if (not (pair? i)) i (car i))) parameters)
	     (list-of-mixed-args parameters arguments env)
	     (procedure-environment procedure)))))
	(else
	 (error
	  "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
	    (list-arg-values (rest-operands exps)
			     env))))

(define (list-of-mixed-args prms exps env)
  (define (on-prms prm exp env)
    (cond ((not (pair? prm)) (actual-value exp env))
	  ((eq? (cdr prm) 'lazy) (delay-it exp env))
	  ((eq? (cdr prm) 'lazy-memo) (delay-it-memo exp env))))
  (if (no-operands? exps)
      '()
      (cons (on-prms (car prms) (first-operand exps) env)
	    (list-of-mixed-args (cdr prms) (rest-operands exps) env))))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (delay-it-memo exp env)
  (list 'thunk-memo exp env))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (force-it obj)
  (cond ((thunk? obj)
	 (actual-value (thunk-exp obj) (thunk-env obj)))
	((thunk-memo? obj)
	 (let ((result (actual-value
			(thunk-exp obj)
			(thunk-env obj))))
	   (set-car! obj 'evaluated-thunk)
	   (set-car! (cdr obj) result)
	   (set-cdr! (cdr obj) '())
	   result))
	((evaluated-thunk? obj)
	 (thunk-value obj))
	(else obj)))

;; other procedures kept unchanged
