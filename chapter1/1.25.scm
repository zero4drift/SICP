(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; this version computes huge intermediate results
;; be attention to procedure square applied in fast-expt and original expmod
