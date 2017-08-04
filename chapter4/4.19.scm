;; ben

((lambda (a)
   (define (f x)
     (define b (+ a x))
     (define a 5)
     (+ a b))
   (f 10))
 1)

;; in this way the output is 16

;; alysssa

((lambda (a)
   (define (f x)
     (let ((b '*unassigned*)
	   (a '*unassigned*))
       (set! b (+ a x))
       (set! a 5)
       (+ a b)))
   (f 10))
 1)

;; in this way an error occurred

;; in theory eva is right, but it is hard to implement
;; then alysssa's idea is more reasonable than ben's
;; if simultaneous internal definitions are necessary and desired

;; the output of ben's way is false
;; reporting an error like alysssa's way is more acceptable.
;; fot that an error is always better than a false result
