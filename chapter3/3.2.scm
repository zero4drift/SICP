(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (if (eq? x 'how-many-calls?)
	  count
	  (begin (set! count (+ count 1))
		 (f x))))))
