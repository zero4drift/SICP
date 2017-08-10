;; in applicative order scheme
;; this factorial would runs into death loop
;; even with the n equals to 1
;; the (* n (factorial (- n 1))) will be computed in applicative order

;; it will pass in normal order scheme
