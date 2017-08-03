(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (eval exp env)
  (let ((tag (type-tag exp)))
    (let ((proc (get 'eval tag)))
      (if proc
	  (proc exp env)
	  (error "Unknown expression type -- EVAL"
		 exp)))))

;; number: scheme-number
;; string: scheme-string
(define (eval-self-evaluation exp env)
  (cadr exp))

(put 'eval 'scheme-number eval-self-evaluation)
(put 'eval 'scheme-string eval-self-evaluation)

;; variable: 'quote
(define (eval-variable exp env)
  (text-of-quotation exp))

(put 'eval 'quote eval-variable)

;; assignment: set!

;; the original eval-assignment
(put 'eval 'set! eval-assignment)

;; definition: define

;; the original eval-definition
(put 'eval 'define eval-definition)

;; lambda: lambda

;; should modify the original make-procedure
(define (make-procedure-lambda exp env)
  (let ((parameters (lambda-parameters exp))
	(body (lambda-body exp)))
    (make-procedure parameters body env)))

(put 'eval 'lambda make-procedure-lambda)

;; if: if

;; the original eval-if
(put 'eval 'if eval-if)

;; begin: begin

;; should modify the original eval-sequence
(define (eval-sequence-begin exp env)
  (let ((actions (begin-actons exp)))
    (eval-sequence actions env)))

(put 'eval 'begin eval-sequence-begin)

;; cond: cond
;; should modify the original handle method
(define (eval-cond exp env)
  (let ((transfer (cond->if exp)))
    (eval transfer env)))

(put 'eval 'cond eval-cond)

;; application: call
;; based on the procedures defined in exercise 4.2 b
(define (eval-application exp env)
  (apply (eval (operator exp) env)
	 (list-of-values (operands exp) env)))

(put 'eval 'call eval-application)
