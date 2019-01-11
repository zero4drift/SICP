;; a

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (if (eq? (car vals) '*unassigned*)
		 (error "Unassigned variable" var)
		 (car vals)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;; b

(define (transfer-defines-let vars bodys)
  (let ((attached-unassigned (map (lambda (i) (list i '*unassigned*)) vars))
	(transfer-set!-body (map (lambda (i j) (list 'set! i j)) vars bodys)))
    (cons 'let attached-unassigned transfer-set!-body)))

(define (internal-defines body)
  (if (null? body)
      '()
      (if (definition? (car body))
	  (cons (car body)
		(internal-defines (cdr body))))))

(define (internal-body body)
  (if (not (definition? (car body)))
      body
      (internal-body (cdr body))))

(define (scan-out-defines body)
  (let ((define-vars (map definition-variable (internal-defines body)))
	(define-bodys (map definition-value (internal-defines body)))
	(inside-body (internal body)))
    (if (null? define-vars)
	inside-body
	(append (transfer-defines-let define-vars define-bodys)
		(scan-out-defines inside-body)))))

;; c

;; prefer to implement it in the make-procedure
;; since most procedures defined would be utilized more than one time
;; so it's more efficient
;; when implement it in procedure-body
;; it would transfer the body every time when call it
