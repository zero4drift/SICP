(define (g y)
  (define (f x)
    (let ((z y))
      (set! y x)
      z))
  f)

(define f (g 0))
