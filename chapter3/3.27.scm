;; when compute (memo-fib n)
;; the previous (memo-fib (- n 1)) (memo-fib (- n 2))
;; are already stored in table
;; we only need to fetch them and plus them
;; when compute (memo-fib (- n 1))
;; repeat the process illustrated before

;; overall the steps needed is proportional to n


;; when transferred the statement to (memoize fib)
;; the process pattern is changed
;; becaus the (fib (- n 1)) and (fib (- n 2))
;; can not look up the previous result in table
;; the setps need is the same to the old fib
