(define base-stream
  (weighted-pairs
   integers
   integers
   (lambda (p) (+ (expt (car p) 3) (expt (cadr p) 3)))))

(define Ramanujan-stream
  (define (get-desired s1 s2)
    (if (= (stream-car s1) 0)
	(cons-stream
	 (stream-car s2)
	 (get-desired
	  (stream-cdr s1)
	  (stream-cdr s2)))
	(get-desired
	 (stream-cdr s1)
	 (stream-cdr s2))))
  (let ((rest-stream (stream-cdr base-stream)))
    (ler ((sub-stream (stream-map
		       -
		       base-stream
		       rest-stream)))
	 (get-desired
	  sub-stream
	  rest-stream))))
	 
