(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (pairs (stream-cdr s) (stream-cdr t))))

;; it does not work
;; due to the absence of delay in (pairs (stream-cdr s) (stream-cdr t))
;; ceased to an infinite loop
