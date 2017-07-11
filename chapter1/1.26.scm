(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (* (expmod base (/ exp 2) m)
		       (expmod base (/exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (-exp 1) m))
		    m))))

;; if * is applied to two parameters instead of procedure square
;; both parameters would be computed
;; only one parameter needs to be computed when square is utilized
