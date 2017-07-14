(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (get-a-b z n count)
  (let ((remain (remainder z n)))
    (if (= remain 0)
	(get-a-b (/ z n) n (+ count 1))
	count)))

(define (car z)
  (get-a-b z 2 0))

(define (cdr z)
  (get-a-b z 3 0))
