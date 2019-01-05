(define (cube-pair p)
  (+ (expt (car p) 3) (expt (cadr p) 3)))

(define base-stream
  (weighted-pairs
   integers
   integers
   cube-pair))

(define (Ramanujan-pairs s1 s2)
  (if (= (cube-pair (stream-car s1))
	 (cube-pair (stream-car s2)))
      (cons-stream (cube-pair (stream-car s1))
		   (Ramanujan-pairs (stream-cdr s1)
				    (stream-cdr s2)))
      (Ramanujan-pairs (stream-cdr s1)
		       (stream-cdr s2))))

(define Ramanujan-streams
  (Ramanujan-pairs base-stream
		   (stream-cdr base-stream)))
