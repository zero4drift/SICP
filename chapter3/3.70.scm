(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< (weight s1car) (weight s2car))
		  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
		 ((> (weight s1car) (weight s2car))
		  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
		 (else
		  (cons-stream s1car
			       (merge-weighted
				(stream-cdr s1)
				s2 weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;; a

(define desired1
  (weighted-pairs
   integers
   integers
   (lambda (p) (+ (car p) (cadr p)))))

;; b

(define s (cons-stream 1 (merge (scale-stream s 2)
				(merge (scale-stream s 3)
				       (scale-stream s 5)))))

(define desired2
  (weighted-pairs
   s
   s
   (lambda (p) (+ (* 2 (car p)) (* 3 (cadr p)) (* 5 (car p) (cadr p))))))
