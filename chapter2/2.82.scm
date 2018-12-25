(define (transfer-1 base element)
  (let ((tag (type-tag element)))
    (if (equal? base tag)
	element
	(let ((proc (get-coercion tag base)))
	  (if proc
	      (proc element)
	      false)))))

(define (iter-transfer base args result)
  (if (null? args)
      result
      (let ((element (car args))
	    (left (cdr args)))
	(let ((transfer-one (transfer-1 base element)))
	  (if transfer-one
	      (iter-transfer base left (cons transfer-one result))
	      false)))))
      

(define (transfer type-tags args)
  (if (null? type-tags)
      #f
      (let ((base (car type-tags))
	    (left-type-tags (cdr type-tags)))
	(let ((result (iter-transfer base args '())))
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
		;; below is a flase form of calling apply-generic
		;; (apply-generic op . args)
		;; should flat the exact second parameter below
		(apply-generic op (reverse transfer-result))
		(error "No method for these types"
		       (list op type-tags))))))))

;; case like that:
;; type1->type2 exists
;; type3->type1 exists
;; worst case: another two transfer ways(transfer to type1, type3) do not work;
;; and type3 could not be transferred to type2, failed at last
;; though there is a way of multi-transfer process of type3 to type1 and then to type2; 
