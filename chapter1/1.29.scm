(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (sum-simpson term count n)
  (if (> count n)
      0
      (+ (term count)
	 (sum-simpson term (+ count 1) n)))) ;this will do, but the sum procedure is better here

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (compute count)
    (cond ((or (= count 0) (= count n)) (f (+ a (* count h))))
	  ((= (remainder count 2) 1) (* 4 (f (+ a (* count h)))))
	  ((= (remainder count 2) 0) (* 2 (f (+ a (* count h)))))))
  (define (count-next count)
    (+ count 1))
  (* (/ h 3)
     (sum compute 0 count-next n)))	;(sum-simpson compute 0 n)

;; integral-simpson's result is more accurate than the former integral procedure
