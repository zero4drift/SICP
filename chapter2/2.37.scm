;; based on a more general procedure map indicated by footnote 83

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (martix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (martix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (martix-*-vector cols v)) m)))
