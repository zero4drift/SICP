(define (product term a next b)
  (if (> a b)
      1
      (*
       (term a)
       (product term (next a) next b))))

;; a

(define (compute-next y)
  (+ y 1))

(define (factorial n)
  (define (keep-same x)
    x)
  (product keep-same 1 compute-next n))

(define (compute-pi n)
  (define (pi-term n)
    (if (even? n)
	(/ (+ n 2) (+ n 1))
	(/ (+ n 1) (+ n 2))))
  (* (product pi-term 1 compute-next n)
     4))

;; b

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result a))))
  (iter a 1))
