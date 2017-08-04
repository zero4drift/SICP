;; the way defined in this exercise won't work
;; the way defined in context will work

;; just transfer the way defined in this exercise to lambda form

(define (solve f y0 dt)
  ((lambda (y dy)
     ((lambda (a b)
	(set! y a)
	(set! dy b))
      (integral (delay dy) y0 dt)
      (stream f y))			;this will fail due to the y is still '*unassigned here
     y)
   '*unassigned*
   '*unassigned*))
