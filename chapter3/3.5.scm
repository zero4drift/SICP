(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (mid-point p1 p2)
  (let ((x (/ (+ (x-point p1) (x-point p2)) 2))
	(y (/ (+ (y-point p1) (y-point p2)) 2)))
    (make-point x y)))

;; there the rectangular is just a square.
(define (radius-square p1 p2)
  (let ((x1 (x-point p1))
	(x2 (x-point p2)))
    (square (abs (/ (- x1 x2) 2)))))

(define (p r1 r2 m1 m2 rs)
  (not (> (+ (square (- r1 m1))
	     (square (- r2 m2)))
	  rs)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 n)
  (let ((p1 (make-point x1 y1))
	(p2 (make-point x2 y2)))
    (let ((center (mid-point p1 p2)))
      (define (integral-test)
	(let ((r1 (random-in-range x1 x2))
	      (r2 (random-in-range y1 y2))
	      (m1 (x-point center))
	      (m2 (y-point center))
	      (rs (radius-square p1 p2)))
	  (p r1 r2 m1 m2 rs)))
      (monte-carlo n integral-test))))

(estimate-integral p 2.0 8.0 4.0 10.0 100000)
