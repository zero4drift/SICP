;; the original one

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; the one always computes the value from left to right

(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
	(cons first
	      (list-of-values-lr (rest-operands exps) env)))))

;; the one always computes the value from right to left

(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values-rl (rest-operands exps) env)))
	(cons (eval (first-operand exps) env)
	      rest-values))))
