;; a

;; look at the predicate below
(define (application? exp) (pair? exp))

;; procedure application is a pair
;; definition and assignment are pairs too (what's more, lambda, if ...)
;; so when the cond arrangements of eval placed like louis's plan
;; definition and assignment are handled as applications ...

;; b

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
