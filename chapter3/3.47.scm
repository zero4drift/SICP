;; a

(define (make-semaphore n)
  (let ((mutex (make-mutex))
	(original-n n))
    (define (acquire)
      (mutex 'acquire)
      (if (> n 0)
	  (begin (set! n (- n 1))
		 (mutex 'release)
		 'ok)
	  (begin (mutex 'release)
		 (acquire))))
    (define (release)
      (if (< n original-n)
	  (begin (mutex 'acquire)
		 (set! n (+ n 1))
		 (mutex 'release)
		 'ok)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (acquire))
	    ((eq? m 'release)
	     (release))))
    the-semaphore))

;; b

(define (make-semaphore n)
  (let ((original-n n))
    (define (acquire)
      (if (test-and-set! n)
	  (acquire)
	  'ok))
    (define (release)
      (if (< n original-n)
	  (begin
	    (set! n (+ n 1))
	    'ok)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (acquire))
	    ((eq? m 'release)
	     (release))))
    the-semaphore))

(define (test-and-set! n)
  (if (= n 0)
      true
      (begin (set! n (- n 1))
	     false)))
