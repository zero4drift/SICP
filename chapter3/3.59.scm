;; a

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (div-streams s1 s2)
  (stream-map / s1 s2))			;no 0 in s2

(define (integrate-series s)
  (define integers (integers-starting-from 1))
  (div-streams s integers))

;; b

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
