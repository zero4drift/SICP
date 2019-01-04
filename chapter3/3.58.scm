;; The result is the rational value that num divides den in base radix(num < den)
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; (expand 1 7 10)
;; (1 4 2 8 5 ...)
;; 1 / 7 = 0.14285...

;; (expand 3 8 10)
;; (3 7 5 0 0 ...)
