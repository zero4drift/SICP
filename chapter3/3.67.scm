(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list x (stream-car t)))
		 (stream-cdr s))
     (stream-map (lambda (x) (list (strem-car s) x))
		 (stream-cdr t)))
    (pairs (stream-cdr s) (stream-cdr t)))))
