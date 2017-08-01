(define (smooth input-stream)
  (stream-map (lambda (i j) (/ (+ i j) 2))
	      (cons-stream 0 input-stream)
	      input-stream))

(define (make-zero-crossings input-stream smooth)
  (let ((smooth-stream (smooth input-stream)))
    (stream-map sign-change-detector
		smooth-stream
		(cons-stream
		 0
		 smooth-stream))))
