(define base-stream
  (weighted-pairs
   integers
   integers
   (lambda (p) (+ (square (car p)) (square (cadr p))))))

(define (square-pair p)
  (+ (square (car p)) (square (cadr p))))

(define desired-pairs
  (define (get-desired s1 s2 s3)
    (if (and (= (stream-car s1) 0) (= (stream-car s2) 0))
	(cons-stream
	 (stream-car s3)
	 (get-desired
	  (stream-cdr s1)
	  (stream-cdr s2)
	  (stream-cdr s3)))
	(get-desired
	 (stream-cdr s1)
	 (stream-cdr s2)
	 (stream-cdr s3))))
  (let ((square-stream (stream-map square-pair base-stream))
	(rest-stream (stream-cdr (stream-cdr base-stream))))
    (let ((sub-stream1 (stream-map
			-
			square-stream
			(stream-cdr square-stream)))
	  (sub-stream2 (stream-map
			-
			(stream-cdr square-stream)
			(stream-cdr (stream-cdr square-stream)))))
      (get-desired sub-stream1 sub-stream2 rest-stream))))

(define desired-numbers
  (stream-map square-pair desired-pairs))
