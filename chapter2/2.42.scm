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

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (safe? k positions)
  (if (= k 1)
      true
      (let ((last (list-ref positions (- k 1)))
	    (test (list-ref positions (- k 2))))
	(let ((last-row (car last)) (test-row (car test)))
	  (and
	   (not (= last-row test-row))
	   (not (= (+ 1 last-row) test-row))
	   (not (= (+ 1 test-row) last-row)))))))
