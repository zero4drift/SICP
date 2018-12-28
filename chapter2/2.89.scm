;; keep other procedures unchanged

(define (first-term-d term-list)
    (make-term (- (length term-list) 1) (car term-list)))

(define (adjoin-term-d term term-list)
    (cond ((=zero? (coeff term)) term-list)
	  ((equ? (order term) (length term-list))
	   (cons (coeff term) term-list))
	  ((> (order term) (length term-list))
	   (adjoin-term-d term (cons 0 term-list))))) ;;the order of term would never be smaller than the length
