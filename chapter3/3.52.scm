(define sum 0)
;; sum 0

(define (accum x)
  (set! sum (+ x sum))
  sum)

;; accum

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; sum 1

(define y (stream-filter even? seq))
;; sum 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
;; sum 10

(stream-ref y 7)
;; 6
;; 10
;; 28
;; 36
;; 66
;; 78
;; 120
;; 136

;; 136
;; sum 136

(display-stream z)
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210

;; sum 210

;; if implement procedure delay without memo-proc;
;; the evaluation of stream seq will recalculate its elements;
;; since we call procedure accum to calculate its element, 
;; the global variable sum would keep changing.
