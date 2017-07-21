(define (transfer-1 base element)
  (let ((tag (type-tag element)))
    (if (equal? base type-tag)
	element
	(let ((proc (get-coercion base tag)))
	  (if proc
	      (proc element)
	      false)))))

(define (recur-transfer base args result)
  (if (null? args)
      result
      (let ((element (car args))
	    (left (cdr args)))
	(let ((transfer-one (transfer-1 base element)))
	  (if transfer-one
	      (recur-transfer base (cdr args) (cons transfer-one result))
	      false)))))
      

(define (transfer type-tags args)
  (if (null? type-tags)
      false
      (let ((base (car type-tags))
	    (left-type-tags (cdr type-tags)))
	(let ((result (recur-transfer base args)))
	  (if result
	      result
	      (transfer left-type-tags args))))))
    

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (let ((transfer-result (transfer type-tags args)))
	    (if transfer-result
		(apply-generic op (reverse transfer-result))
		(error "No method for these types"
		       (list op type-tags))))))))

;; case like that:
;; type1->type2 exists
;; type3->type1 exists
;; in this procedure, type3 could not be transferred to type2, failed at last
