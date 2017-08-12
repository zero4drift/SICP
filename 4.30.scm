;; a
;; because the procedure body of (lambda (x) (newline) (display x)) has two primitive procedures
;; when evaluated using eval, it will call actual-value to get x

;; b
;; the original one
;; (p1 1) => (1 2)
;; (p2 1) => 1 (set! x (cons x '(2))) would be delayed in (p (set! x (cons x '(2))))

;; cy's version
;; (p1 1) => (1 2)
;; (p2 1) => (1 2)

;; c
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))				;when the exp is not a thunk, just return it


;; define
;; I prefer cy's way.
