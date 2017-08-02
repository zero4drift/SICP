;; order-stream (('generate) ('reset 124) ...)

(define (base-stream base)
  (define rand-stream
    (cons-stream
     base
     (stream-map rand-update rand-stream)))
  rand-stream)

(define (random-numbers order-stream rand-stream)
  (cond ((eq? (car (stream-car order-stream)) 'generate)
	 (cons-stream
	  (stream-car (rand-stream))
	  (random-numbers
	   (stream-cdr order-stream)
	   (stream-cdr rand-stream))))
	((eq? (car (stream-car order-stream)) 'reset)
	 (random-numbers
	  (stream-cdr order-stream)
	  (base-stream (cdr (stream-car order-stream)))))))

(define (random-numbers-order order-stream)
  (random-numbers order-stream (base-stream random-init)))
