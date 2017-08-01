(define (cube-pair pair)
  (+ (expt (car p) 3) (expt (cadr p) 3)))

(define base-stream
  (weighted-pairs
   integers
   integers
   cube-pair))

(define Ramanujan-pairs
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
  (let ((rest-stream (stream-cdr base-stream))
	(cube-streams (stream-map cube-pair base-stream)))
    (ler ((sub-stream (stream-map
		       -
		       cube-streams
		       (stream-cdr cube-streams))))
	 (get-desired
	  sub-stream
	  rest-stream))))

(define Ramanujan-streams
  (stream-map cube-pair Ramanujan-pairs))
