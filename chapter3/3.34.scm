(define (squarer a b)
  (multiplier a a b))

;; set value of a, the value of b could be computed;
;; while set value of b, the value of a could not be.
;; because when we want to compute the 2nd root of b,
;; we must set value of b and make a forget its value(or the new value could not be set),
;; the cond predicate statements of procedure process-new-value of multiplier all failed,
;; it is impossibile to set a new value for a.
