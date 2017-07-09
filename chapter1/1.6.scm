(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))		;new-if is not an special form operation
					;would eval the three parameters of new-if at the same time
					;break the maximum depth of recursion
