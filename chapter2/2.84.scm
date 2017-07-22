(define (install-types-tower)
  (let ((types-tower (list 'scheme-number
			   'rational
			   'real
			   'complex)))
    (put 'types-tower 'number types-tower)))

(define (get-number-higher-type type)
  (let ((types-tower (get 'types-tower 'number)))
    (let ((sub-types-tower (memq type types-tower)))
      (if sub-types-tower
	  (cadr sub-types-tower)
	  (error "No higher type for this type" type)))))

(define (lower-one type1 type2)
  (let ((types-tower (get 'types-tower 'number)))
    (let ((left-type1-tower (memq type1 types-tower))
	  (left-type2-tower (memq type2 type-tower)))
      (if (and left-type1-tower left-type2-tower)
	  (> (length left-type1-tower) (length left-type2-tower))
	  (error "Invalid type" type1 type2)))))

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
		(cond ((equal? type1 type2)
		       (error "No method for these types"
			      (list op type-tags)))
		      ((lower-one t1 t2)
		       (apply-generic op (raise a1) a2))
		      (else (apply-generic op a1 (raise a2)))))
	      (error "No method for these types"
		     (list op type-tags)))))))
  
;; install types tower in one of  the official answers

(define (install-level-package)
  (put 'level 'scheme-number 1)
  (put 'level 'rational 2)
  (put 'level 'real 3)
  (put 'level 'complex 4)
  (put 'level 'rectangular 4)
  (put 'level 'polar 4)
  'done)

;; corresponding procedure based on this is much more efficient than mine
;; but I will keep my ugly answer
