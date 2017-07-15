(define (make-cent-percent c p)
  (let ((w (* c p)))
    (make-interval (- c w) (+ c w))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (/ (width i) (center i)))

;; when procedure make-cent-percent  and a very amount percent are applied to construct a interval
;; the result of div-interval applied to such intervals is approaching true
