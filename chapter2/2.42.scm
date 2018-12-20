;; (load "2.33.scm")
;; (load "2.40.scm")
;; (load "2.41.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (make-position row k)
  (list k row))

(define (row-position p)
  (cadr p))

(define (k-position p)
  (car p))

(define (adjoin-position new-row k rest-of-queens)
  (cons (make-position new-row k) rest-of-queens))

(define (get-row positions k)
  (cond ((null? (car positions)) (error "invalid operation" k))
	((= (k-position (car positions)) k) (row-position (car positions)))
	(else (get-row (cdr positions) k))))

(define (safe? k positions)
  (if (= k 1)
      'true
      (let ((r1 (get-row positions k))
	    (r2 (get-row positions (- k 1))))
	(and
	 (not (= r1 r2))
	 (not (= (+ 1 r1) r2))
	 (not (= (+ 1 r2) r1))))))
