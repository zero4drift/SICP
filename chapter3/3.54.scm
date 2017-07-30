(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))
