(define (ln2-summands n)
  (cons-stream
   (/ 1.0 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))	;first way

(define ln2-stream-euler
  (euler-transfrom ln2-stream))		;second way

(define ln2-stream-accelerated
  (accelerated-sequence euler-transfrom ln2-stream)) ;third way
