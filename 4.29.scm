(define (fibonacci n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fibonacci (- n 1))
		 (fibonacci (- n 2))))))
;; better with memo-proc


(define (square x)
  (* x x))

;; with memo
;; (square (id 10))
;; 100

;; count
;; 1

;; without memo
;; (square (id 10)
;; 100

;; count
;; 2
