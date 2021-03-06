(define (or-gate a1 a2 output)
  (let ((invert-1 (make-wire))
	(invert-2 (make-wire))
	(and-invert1-invert2 (make-wire)))
    (inverter a1 invert-1)
    (inverter a2 invert-2)
    (and-gate invert-1 invert-2 and-invert1-invert2)
    (inverter and-invert1-invert2 output))
  'ok)

;; concurrent case, delay time is hard to determine
;; about (+ (* 3 inverter-delay) and-gate-delay)
