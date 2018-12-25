;; a

;; for exp with scheme-number this will do its job
;; for exp with complex it exceeds maximum recursion depth


;; b

;; no, it induces another problem of recursion depth exceeding
;; apply-generic procedure would not work properly,
;; with two same parameters and the corresponding transfer procedure installed
;; it's operation exceeds the maximum recursion depth


;; c

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if(equal? type1 type2)
		   (error "No method for these types"
			  (list op type-tags))
		   (let ((t1->t2 (get-coercion type1 type2))
			 (t2->t1 (get-coercion type2 type1)))
		     (cond (t1->t2
			    (apply-generic op (t1->t2 a1) a2))
			   (t2->t1
			    (apply-generic op a1 (t2->t1 a2)))
			   (else
			    (error "No method for these types"
				   (list op type-tags))))))
		(error "No method for these types"
		       (list op type-tags))))))))
