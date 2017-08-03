;; first eval self-defined special procedure through eval procedure

;; show the structure of while by making a definition
(define (while exp proc)
  (if exp
      (begin proc
	     (while exp proc))
      'finished))
;; so while is composed of if and begin

(define (while? exp)
  (tagged-list? exp 'while))

(define (while-predicate exp)
  (cadr exp))

(define (while-actions exp)
  (make-begin
   (list (caddr exp)
	 exp)))

(define while-alternative
  'finished)

(define (while-transfer exp)
  (make-if
   (while-predicate exp)
   (while-actions exp)
   while-alternative))

(define (eval-while exp env)
  (eval (while-transfer exp) env))

;; just implement while? and eval-while in eval
