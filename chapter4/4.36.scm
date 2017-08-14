;; only an-integer-starting-from is utilized

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
	(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))

;; initial status i = j = k
;; of course the initial i j k will fail
;; but will never stop
;; cause (let ((k (an-integer-starting-from i)))) would never stop
;; and no value of k could satisfy the equation

(define (a-pythagorean-triple-between low high)
  (let ((k (an-integer-starting-from low)))
    (let ((i (an-integer-between low k)))
      (let ((j (an-integer-between i k)))
	(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))
