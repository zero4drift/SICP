(define count 0)

(define (cube x)
  (* x x x))

(define (p x)
  (set! count (+ count 1))
  (newline)
  (display count)			;a trick to show how many times p applied
  (newline)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)				;times of p called: 5
					;order of growth of space: O(log a)
					;(sine a) order of growth of number of steps: O(log a)
