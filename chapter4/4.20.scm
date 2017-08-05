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

;; the ordinary let

(let ((fact
       (lambda (n)
	 (if (= n 1)
	     1
	     (* n (fact (- n 1)))))))
  (fact 10))

;; could be transferred to lambda

((lambda (fact)
   (fact 10))
 (lambda (n)
   (if (= n 1)
       1
       (* n (fact (- n 1))))))

;; thus, in the definition body of the second lambda
;; the binding of the variable fact could not be found
;; the env structure illustration could be ignored


;; the letrec

(letrec ((fact
       (lambda (n)
	 (if (= n 1)
	     1
	     (* n (fact (- n 1)))))))
  (fact 10))

;; could be transferred to let with 'unassigned & set!

(let ((fact 'unassigned))
  (set! fact
	(lambda (n)
	  (if (= n 1)
	      1
	      (* n (fact (- n 1))))))
  (fact 10))

((lambda (fact)
   (set! fact				;rewrite the variable fact's binding in outer env
	 (lambda (n)
	   (if (= n 1)
	       1
	       (* n (fact - n 1)))))	;when eval the lambda body, the variable fact's binding value is a procedure
   (fact 10))
 'unassigned)

;; sure it would pass
;; the env structure ignored
