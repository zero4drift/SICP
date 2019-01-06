;; order-stream (('generate) ('reset 124) ...)

(define (random-numbers order-stream val)
  (define (base-stream base)
    (cons-stream
     base
     (random-numbers
      (stream-cdr order-stream)
      (rand-update base))))
  (cond ((eq? (car (stream-car order-stream)) 'generate)
	 (base-stream val))
	((eq? (car (stream-car order-stream)) 'reset)
	 (base-stream (cadr (stream-car order-stream))))
	(else "Invalid order" (stream-car order-stream))))
