;; a

(define (unless? exp)
  (tagged-list? exp 'unless))

(define (unless-condition exp)
  (cadr exp))

(define (unless-usual-value exp)
  (caddr exp))

(define (unless-exceptional-value exp)
  (cadddr exp))

(define (unless-if exp)
  (let ((condition (unless-condition exp))
	(usual-value (unless-usual-value exp))
	(usual-exceptional-value exp))
    (make-if condition usual-exceptional-value usual-value)))

(define (eval-unless exp env)
  (eval (unless-if exp) env))

;; implemetn unless? and eval-unless in eval

;; b
;; a case which is about high order procedure...
