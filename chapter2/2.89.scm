;; keep other procedures unchanged

(define (first-term term-list)
  (make-term (- (len term-list) 1) (car term-list)))

(define (adjoin-term term term-list)
  (cond ((=zero? (coeff term)) term-list)
	((=equ? (order term) (length term-list))
	 (cons (coeff term) term-list))
	((> (order term) (length term-list))
	 (adjoin-term term (cons 0 term-list))))) ;the order of term would never be smaller than the length
