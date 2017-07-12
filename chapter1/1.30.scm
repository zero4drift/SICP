(define (cube x)
  (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (compute count)
    (cond ((or (= count 0) (= count n)) (f (+ a (* count h))))
	  ((= (remainder count 2) 1) (* 4 (f (+ a (* count h)))))
	  ((= (remainder count 2) 0) (* 2 (f (+ a (* count h)))))))
  (define (count-next count)
    (+ count 1))
  (* (/ h 3)
     (sum compute 0 count-next n)))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))				;iterative

