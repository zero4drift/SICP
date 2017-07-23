;; common procedures for two types of terms

;; the-empty-termlist
;; rest-terms
;; empy-termlist?
;; make-term
;; coeff
;; order

(define (install-dense-terms-package)
  ;; procedures about dense terms...
  (define (tag terms)
    (attach-tag 'dense terms))
  (define (minus-t terms)
    (if (empty-termlist? terms)
	the-empty-termlist
	(let ((first (first-term terms)))
	  (adjoin-term (make-term
			(order term)
			(minus (coeff term)))
		       (minus-t (rest-terms terms))))))
  (put 'make 'dense
       (lambda (x) (tag x)))
  (put 'minus 'dense
       (lambda (x) (tag (minus x))))
  (put 'first-term
       (lambda (x) (first-term x)))
  (put 'adjoin-term '(dense)
       (lambda (x y) (tag (adjoin-term x y)))))

(define (make-dense-terms terms)
  ((get 'make 'dense terms) terms))

(define (install-sparse-terms-package)
  ;; procedures about sparse terms...
  (define (tag terms)
    (attach-tag 'sparse terms))
  (define (minus-t terms)
    (if (empty-termlist? terms)
	(the-empty-termlist)
	(adjoin-term
	 (make-term
	  (minus (first-term terms)))
	 (minus-t (rest-terms terms)))))
  (put 'make 'sparse
       (lambda (x) (tag x)))
  (put 'minus 'sparse
       (lambda (x) (tag (minus-t x))))
  (put 'first-term 'sparse
       (lambda (x) (first-term x)))
  (put 'adjoin-term 'sparse
       (lambda (x y) (tag (adjoin-term x y)))))

(define (make-sparse-terms terms)
  ((get 'make 'sparse) terms))
  

(define (install-polynomial-package)
  ;; internal procedures...
  (put 'sub '(poly poly)
       (lambda (x y) (tag 'poly (sub-poly x y)))) ;2.88
  ;; others kept unchanged
  )

(define (first-term x)
  (apply-generic 'first-term x))

(define (adjoin-term x y)
  ((get 'adjoin-term (tag-type y)) x (contents y)))

(define (minus x)
  (apply-generic 'minus x))
