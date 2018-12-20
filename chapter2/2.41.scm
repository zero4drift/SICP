;; (load "2.33.scm")
;; (load "2.40.scm")

(define (filter p s)
  (cond ((null? s) '())
	((p (car s))
	 (cons (car s) (filter p (cdr s))))
	(else (filter p (cdr s)))))

(define (triple-pair-sum p)
  (accumulate + 0 p))

(define (triple-pair n)
  (let ((range (enumerate-interval 1 n)))
    (flatmap
     (lambda (a)
       (flatmap
	(lambda (b)
	  (map (lambda (c) (list c b a))
	       (enumerate-interval (+ b 1) n)))
	(enumerate-interval (+ a 1) n)))
     range)))

(define (triple-pair-sum-n n s)
  (filter
   (lambda (p) (= s (triple-pair-sum p)))
   (triple-pair n)))
