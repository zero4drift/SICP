(define (func-re n)
  (if (< n 3)
      n
      (+ (func-re (- n 1)) (* 2 (func-re (- n 2))) (* 3 (func-re (- n 3))))))

(define (func-it n)
  (func-iter 2 1 0 0 n))

(define (func-iter a b c i n)
  (if (= i n)
      c
      (func-iter (+ a (* 2 b) (*3 c))
		 a
		 b
		 (+ i 1)
		 n)))
