(define (make-deque)
  (cons '() '()))

(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (set-front-ptr! deque item)
  (set-car! deque item))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (make-pair item front rear)
  (cons item (cons front rear)))

(define (front-rear-pair pair)
  (cdr pair))

(define (set-pair-front pair front)
  (set-car! (front-rear-pair pair) front))

(define (set-pair-rear pair rear)
  (set-cdr! (front-rear-pair pair) rear))

(define (pair-item pair)
  (car pair))

(define (pair-rear pair)
  (cdr (front-rear-pair pair)))

(define (pair-front pair)
  (car (front-rear-pair pair)))

(define (front-insert-deque! deque item) ;insert to the front
  (let ((new-pair (make-pair item '() '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-pair-front (front-ptr deque) new-pair)
	   (set-pair-rear new-pair (front-ptr deque))
	   (set-front-ptr! deque new-pair)
	   (display-deque deque)))))

(define (rear-insert-deque! deque item)	;insert to the end
  (let ((new-pair (make-pair item '() '())))
    (cond ((empty-deque?
	    deque)
	   (set-rear-ptr! deque new-pair)
	   (set-front-ptr! deque new-pair)
	   deque)
	  (else
	   (set-pair-rear (rear-ptr deque) new-pair)
	   (set-pair-front new-pair (rear-ptr deque))
	   (set-rear-ptr! deque new-pair)
	   (display-deque deque)))))

(define (front-delete-deque! deque)	;delete the front one
  (cond ((empty-deque? deque)
	  (error "DELETE! called with an empty deque" deque))
	(else
	 (set-front-ptr! deque (pair-rear (front-ptr deque)))
	 (set-pair-front (front-ptr deque) '())
	 (display-deque deque))))

(define (rear-delete-deque! deque)	;delete the rear one
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	 (set-rear-ptr! deque (pair-front (rear-ptr deque)))
	 (set-pair-rear (rear-ptr deque) '())
	 (display-deque deque))))

(define (display-deque deque)
  (define (fetch-item ptr)
    (if (null? ptr)
	'()
	(cons (pair-item ptr)
	    (fetch-item (pair-rear ptr)))))
  (if (not (empty-deque? deque))
      (let ((front (front-ptr deque)))
	(display (fetch-item front)))
      '()))
