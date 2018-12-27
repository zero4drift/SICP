;; install common polynomial term package
(define (install-poly-term-package)
  ;; procedures kept same
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;; tag
  (define (tag term)
    (attach-tag 'polynomial-term term))
  ;; put
  (put 'make 'polynomial-term
       (lambda (x y) (tag (make-term x y))))
  (put 'order '(polynomial-term) order)
  (put 'coeff '(polynomial-term) coeff)
  )
(define (make-term x y)
  ((get 'make 'polynomial-term) x y))
(define (order x)
  (apply-generic 'order x))
(define (coeff x)
  (apply-generic 'coeff x))

(define (install-dense-terms-package)
  ;; procedures about dense terms...
  
  ;; prcedures kept same:
  ;; rest-terms
  ;; empy-termlist?
  
  ;; and delete procedure the-empty-termlist;
  ;; replace call of the-empty-termlist with
  ;; 'L1' or 'L' in 'mul-term' & 'mul-term-by-all-terms'
  (define (tag terms)
    (attach-tag 'polynomial-dense terms))
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
    (put 'negative '(polynomial-dense)
	 (lambda (x) (tag (negative-t x))))
    (put 'first-term '(polynomial-dense)
	 (lambda (x) (first-term x)))
    (put 'empty-termlist? '(polynomial-dense) empty-termlist?)
    (put 'rest-terms '(polynomial-dense)
	 (lambda (x) (tag (rest-terms x))))
    (put 'adjoin-term 'polynomial-dense
	 (lambda (x y) (tag (adjoin-term x y))))))


(define (install-sparse-terms-package)
  ;; procedures about sparse terms...

  ;; procedure rest-terms? and so on, define and put
  ;; prcedures kept same:
  ;; rest-terms
  ;; empy-termlist?
  
  ;; and delete procedure the-empty-termlist;
  ;; replace call of the-empty-termlist with
  ;; 'L1' or 'L' in 'mul-term' & 'mul-term-by-all-terms'

  (define (tag terms)
    (attach-tag 'polynomial-sparse terms))
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
  (put 'negative '(polynomial-sparse)
       (lambda (x) (tag (negative-t x))))
  (put 'first-term '(polynomial-sparse)
       (lambda (x) (first-term x)))
  (put 'empty-termlist? '(polynomial-sparse) empty-termlist?)
  (put 'rest-terms '(polynomial-sparse)
       (lambda (x) (tag (rest-terms x))))
  (put 'adjoin-term 'polynomial-sparse
       (lambda (x y) (tag (adjoin-term x y)))))


(define (first-term x)
  (apply-generic 'first-term x))

(define (adjoin-term x y)
  ((get 'adjoin-term (type-tag y)) x (contents y)))

(define (negative x)
  (apply-generic 'negative x))

(define (empty-termlist? x)
  (apply-generic 'empty-termlist x))

(define (rest-terms x)
  (apply-generic 'rest-terms x))


(define (install-polynomial-package)
  ;; internal procedures...
  
  ;; replace call of procedure the-empty-termlist with
  ;; 'L1' or 'L' in 'mul-term' & 'mul-term-by-all-terms'
  ;; add-terms
  ;; mul-terms
  ;; sub-terms

  ;; others kept same

  (put 'sub '(poly poly)
       (lambda (x y) (tag 'polynomial (sub-poly x y)))) ;2.88
  ;; and so on.
  )
