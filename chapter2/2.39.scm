(define (reverse-right sequence)
  (accumulate (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
