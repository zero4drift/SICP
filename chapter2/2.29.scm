;; a

(define (left-branch item)
  (car item))

(define (right-branch item)
  (car (cdr item)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; b

(define (total-weight item)
  (define (iter item result)
    (cond ((null? item) result)
	  ((pair? (car item)) (iter (right-branch item) (total-weight (left-branch item))))
	  (else (+ (branch-structure item) result))))
  (iter item 0))

;; c

(define (total-moment item)
  (define (iter item result)
    (cond ((null? item) result)
	  ((number? item) item)
	  ((pair? (car item)) (iter (right-branch item) (total-moment (left-branch item))))
	  ((pair? item) (+ (* (branch-length item) (total-moment (branch-structure item))) result))))
  (iter item 0))

(define (balance? item)
  (if (not (pair? (car item)))
      true
      (let ((left (left-branch item)) (right (right-branch item)))
	(if (= (total-moment left) (total-moment right))
	    (and (balance? left) (balance? right))
	    false))))

;; d

;; just modify right-branch and branch-structure

(define (right-branch-mod item)
  (cdr item))

(define (branch-structure-mod branch)
  (cdr branch))

