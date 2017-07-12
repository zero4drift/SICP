(define (f g)
  (g 2))

(f f)
;; leads to (f 2)
;; leads to (2 2)
;; error, the object 2 is not applicable.
