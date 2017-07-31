(define fibs
  (cons-stream 0
	       (cons-stream 1
			    (add-streams (stream-cdr fibs)
					 fibs))))

;; same as exercise 3.27
;; if without memo-proc
;; the previous computed results are not cached
;; when there is a need to compute the nth nunber of fibs
;; the previous (- n 1) numbers of fibs still needed to be computed again
;; the computeing steps are accumulated during the processing
;; just the original fib example in page 24
;; whose running steps is increasing exponentially
