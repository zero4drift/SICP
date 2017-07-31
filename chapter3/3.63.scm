(define (sqrt-stream x)
  (cons-stream 1.0
	       (stream-map (lambda (guess)
			     (sqrt-improve guess x))
			   (sqrt-stream x))))

;; it's low efficient
;; due to when there is a need to compute the next element of (sqrt-stream x)
;; it has to fully compute the former elements of (sqrt-stream x) from beginning
;; a new (sqrt-stream x) has no caching

;; when replace the delay implementation
;; the one that takes guess as an internal variable is low efficient as this one
;; cause there is no caching
