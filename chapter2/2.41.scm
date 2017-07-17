(define (triple-pair-sum pair)
  (+ (car pair) (cadr pair) (caddr pair)))

(define (triple-pair n)
  (let ((range (enumerate-interval 1 n)))
    (flatmap
     (lambda (a)
       (map (lambda (x) (append (list a)  x))
	    (flatmap
	     (lambda (b)
	       (map (lambda (y) (list b y))
		    (enumerate-interval (+ b 1) n)))
	     (enumerate-interval (+ a 1) n))))
     range)))

(define (triple-pair-sum-n n s)
  (filter
   (lambda (i) (= s (triple-pair-sum i)))
   (triple-pair n)))
