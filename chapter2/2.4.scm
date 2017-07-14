(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;; applicative mode
;; (cons x y) > (lambda (m) (m x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) q))
;; ((lambda (p q) p) x y)
;; x
