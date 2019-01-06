(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (rand-numbers a1 a2)
  (cons-stream
   (random-in-range a1 a2)
   (random-numbers a1 a2)))

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

(define (estimate-integral x1 x2 y1 y2)
  (define x-random-stream (rand-numbers x1 x2))
  (define y-random-stream (rand-numbers y1 y2))
  (define p1 (make-point x1 y1))
  (define p2 (make-point x2 y2))
  (define center (mid-point p1 p2))
  (define m1 (x-point center))
  (define m2 (y-point center))
  (define rs (radius-square p1 p2))
  (define (integral-test r1 r2)
    (p r1 r2 m1 m2 rs))
  (define results-stream
    (stream-map
     integral-test
     x-random-stream
     y-random-stream))

  (monte-carlo results-stream 0 0))
