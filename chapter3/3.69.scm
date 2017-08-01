(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
		(stream-cdr (pairs t u)))
    (triples (stream-cdr s)
	     (stream-cdr t)
	     (strem-cdr u)))))

(define desired
  (let ((base-stream (triples integers integers integers)))
    (stream-filter
     (lambda (x) (= (square (caddr x)) (+ (square (car x)) (square (cadr x)))))
     base-stream)))
