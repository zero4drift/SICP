;; the former version of this answer is totally wrong
;; due to the misunderstanding of exercise 2.29.

;; a

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

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
  (define (recursive item)
    (cond ((null? item) 0)
	  ((number? item) item)
	  (else (+ (recursive (branch-structure (left-branch item)))
		   (recursive (branch-structure (right-branch item)))))))
  (recursive item))

;; c

(define (total-moment item)
  (* (branch-length item) (total-weight (branch-structure item))))

(define (balance? item)
  (if (not (pair? item))
      true
      (let ((left (left-branch item)) (right (right-branch item)))
	(and (= (total-moment left) (total-moment right))
	     (balance? (branch-structure left))
	     (balance? (branch-structure right))))))

;; d

(define (make-mobile-mod left right)
  (cons left right))

(define (make-branch-mod length structure)
  (cons length structure))


;; just modify right-branch and branch-structure

(define (right-branch-mod item)
  (cdr item))

(define (branch-structure-mod branch)
  (cdr branch))

