(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in a c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (ripple-carry-adder a b s c)
  (let ((c0 (make-wire))
	(c1 (make-wire)))
    (set-signal! c0 0)
    (define (recursive a b c-in sum c-out)
      (if (null? (cdr a))
	  (begin
	    (full-adder (car a) (car b) c-in (car s) c)
	    'ok)
	  (begin (full-adder (car a) (car b) c-in (car s) c-out)
		 (let ((c-in c-out)
		       (c-out (make-wire)))
		   (recursive (cdr a) (cdr b) c-in (cdr s) c-out)))))
    (recursive a b c0 s c1)))

;; this structure's delay is approximately
;; let ((half-adder-delay (+ or-gate-delay (* 2 and-gate-delay) inverter-delay)))
;; let ((full-adder-delay (+ (* 2 half-adder-delay) or-gate-delay)))
;; let ((ripple-darry-adder-n-delay (* n full-adder-delay)))
