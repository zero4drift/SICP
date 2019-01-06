;; mathematical permutations

(define (RLC r l c dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (- (/ 1 c))))
    (define dil
      (add-streams
       (scale-stream vc (/ 1 l))
       (scale-stream il (- (/ r l)))))
    (stream-map cons vc il)))
