(define (square-pair p)
  (+ (square (car p)) (square (cadr p))))

(define base-stream
  (weighted-pairs
   integers
   integers
   square-pair))

(define (desired-pairs s1 s2 s3)
  (if (= (square-pair (stream-car s1))
	 (square-pair (stream-car s2))
	 (square-pair (stream-car s3)))
      (cons-stream (list (square-pair (stream-car s1))
			 (stream-car s1)
			 (stream-car s2)
			 (stream-car s3))
		   (desired-pairs (stream-cdr s1)
				  (stream-cdr s2)
				  (stream-cdr s3)))
      (desired-pairs (stream-cdr s1)
		     (stream-cdr s2)
		     (stream-cdr s3))))

(define desired-streams
  (desired-pairs base-stream
		 (stream-cdr base-stream)
		 (stream-cdr (stream-cdr base-stream))))

;; (325 (1 18) (6 17) (10 15))
;; (425 (5 20) (8 19) (13 16))
;; ...
