(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat-better n d)
  (let ((g (gcd n d)))
    (cond ((< (/ n d) 0) (cons (- (/ (abs n) g)) (/ (abs d) g)))
	  (else ( cons (/ n g) (/ d g))))))
