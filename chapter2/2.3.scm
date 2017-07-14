(define (make-rectangle p1 p2)
  (let ((p3 (make-point (x-point p1) (y-point p2)))
	(p4 (make-point (x-point p2) (y-point p1))))
    (let ((seg1 (make-segment p1 p3))
	  (seg2 (make-segment p1 p4)))
      (cons seg1 seg2))))

(define (width rectangle)
  (let ((width-segment (cdr rectangle)))
    (let ((start (car width-segment))
	  (end (cdr width-segment)))
      (abs (- (x-point end) (x-point start))))))

(define (length rectangle)
  (let ((length-segment (car rectangle)))
    (let ((start (car length-segment))
	  (end (cdr length-segment)))
      (abs (- (y-point end) (y-point start))))))

(define (perimeter rectangle)
  (* 2
     (+ (width rectangle)
	(length rectangle))))

(define (size rectangle)
  (* (width rectangle)
     (length rectangle)))

;;another way

(define (make-rectangle-another start width length)
  (let ((x-start (x-point start))
	(y-start (y-point start)))
    (let ((seg1 (make-segment start (make-point x-start (+ y-start length))))
	  (seg2 (make-segment start (make-point (+ x-start width) y-start))))
      (cons seg1 seg2))))
