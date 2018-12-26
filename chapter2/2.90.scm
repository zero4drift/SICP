;; common procedures for two types of terms

;; the-empty-termlist
;; rest-terms
;; empy-termlist?
;; make-term
;; coeff
;; order

(define (install-dense-terms-package)
  ;; procedures about dense terms...
  ;; procedure rest-terms? and so on, define and put
  (define (tag terms)
    (attach-tag 'dense terms))
  (define (first-term term-list)
    (make-term (- (len term-list) 1) (car term-list)))
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
	  ((=equ? (order term) (length term-list))
	   (cons (coeff term) term-list))
	  ((> (order term) (length term-list))
	   (adjoin-term term (cons 0 term-list))))) ;
  (define (negative-t terms)
    (if (empty-termlist? terms)
	the-empty-termlist
	(let ((first (car terms)))
	  (const (negative first)
		 negative-t (rest-terms terms))))
    (put 'make 'dense
	 (lambda (x) (tag x)))
    (put 'negative '(dense)
	 (lambda (x) (tag (negative x))))
    (put 'first-term '(dense)
	 (lambda (x) (first-term x)))
    (put 'adjoin-term 'dense
	 (lambda (x y) (tag (adjoin-term x y))))))

(define (make-dense-terms terms)
  ((get 'make 'dense terms) terms))


(define (install-sparse-terms-package)
  ;; procedures about sparse terms...
  ;; procedure rest-terms? and so on, define and put
  (define (tag terms)
    (attach-tag 'sparse terms))
  (define (first-term term-list) (car term-list))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (negative-t terms)
    (if (empty-termlist? terms)
	the-empty-termlist
	(let ((first (first-term terms)))
	  (adjoin-term (make-term
			(order first)
			(negative (coeff first)))
		       (negative-t (rest-terms terms))))))
  (put 'make 'sparse
       (lambda (x) (tag x)))
  (put 'negative '(sparse)
       (lambda (x) (tag (negative-t x))))
  (put 'first-term '(sparse)
       (lambda (x) (first-term x)))
  (put 'adjoin-term 'sparse
       (lambda (x y) (tag (adjoin-term x y)))))

(define (make-sparse-terms terms)
  ((get 'make 'sparse) terms))


(define (first-term x)
  (apply-generic 'first-term x))

(define (adjoin-term x y)
  ((get 'adjoin-term (type-tag y)) x (contents y)))

(define (negative x)
  (apply-generic 'negative x))

;; add-terms
;; mul-terms
;; sub-terms
;; add-poly
;; mul-poly
;; sub-poly

(define (install-polynomial-package)
  ;; internal procedures...
  (put 'sub '(poly poly)
       (lambda (x y) (tag 'poly (sub-poly x y)))) ;2.88
  ;; others kept unchanged
  )
