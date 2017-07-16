(define (for-each proc items)
  (if (null? items)
      true
      (begin (proc (car items))
	     (for-each proc (cdr items)))))
