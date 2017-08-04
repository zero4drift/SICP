(define (unbound? exp)
  (tagged-list? exp 'unbound))

(define (unbound-var exp)
  (cadr exp))

(define (make-unbound! var)
  (list 'unbound var))

(define (eval-unbound exp env)
  (unbound-variable! (unbound-var exp) env))

(define (unbound-variable! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     'ok)
	    ((eq? var (car vars))
	     (begin (set! vars (cdr vars))
		    (set! vals (cdr vals))))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;; only delete the matched binding in the current environment
;; cause that the same-name binding in outside environments
;; may be utilized by other procedures
;; it is a disaster for these procedures if all unbounded
