;; (load "./1.20.scm")
;; (load "./1.21.scm")

(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
	((filter a)
	 (combiner (term a)
		   (filtered-accumulate filter combiner null-value term (next a) next b)))
	(else (filtered-accumulate filter combiner null-value term (next a) next b))))

(define (keep-same n)
    n)

(define (compute-next n)
  (+ n 1))

;; a

(define (prime-sum a b)
  (filtered-accumulate prime? + 0 keep-same a compute-next b))

;; b

(define (gcd-product n)
  (define (gcd-equals-1? a)
    (= (gcd a n) 1))
  (filtered-accumulate gcd-equals-1? * 1 keep-same 1 compute-next n))
