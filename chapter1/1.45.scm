;; (load "./1.35.scm")
;; (load "./1.43.scm")

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (root n x)
  (fixed-point
   ((repeated
     average-damp
     (quotient n 2))
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))
