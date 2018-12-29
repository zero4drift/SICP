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
  'done)
(define (make-term x y)
  ((get 'make 'polynomial-term) x y))
(define (order x)
  (apply-generic 'order x))
(define (coeff x)
  (apply-generic 'coeff x))

(define (install-dense-terms-package)
  ;; procedures about dense terms...
  (define (tag terms)
    (attach-tag 'polynomial-dense terms))
  (define (first-term-d term-list)
    (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms-d term-list) (cdr term-list))
  (define (empty-termlist-d? term-list) (null? term-list))
  (define (adjoin-term-d term term-list)
    (cond ((=zero? (coeff term)) term-list)
	  ((equ? (order term) (length term-list))
	   (cons (coeff term) term-list))
	  ((> (order term) (length term-list))
	   (adjoin-term-d term (cons 0 term-list))))) ;
  (define (negative-d terms)
    (if (empty-termlist-d? terms)
	terms
	(let ((first (car terms)))
	  (cons (negative first)
		(negative-d (rest-terms-d terms))))))
  (put 'negative '(polynomial-dense)
       (lambda (x) (tag (negative-d x))))
  (put 'first-term '(polynomial-dense)
       (lambda (x) (first-term-d x)))
  (put 'empty-termlist? '(polynomial-dense) empty-termlist-d?)
  (put 'rest-terms '(polynomial-dense)
       (lambda (x) (tag (rest-terms-d x))))
  (put 'adjoin-term 'polynomial-dense
       (lambda (x y) (tag (adjoin-term-d x y))))
  (put 'make 'polynomial-dense (lambda (t) (tag t)))
  'done)

(define (make-dense-terms terms)
  ((get 'make 'polynomial-dense) terms))

(define (install-sparse-terms-package)
  ;; procedures about sparse terms...
  (define (tag terms)
    (attach-tag 'polynomial-sparse terms))
  (define (first-term-s term-list) (car term-list))
  (define (adjoin-term-s term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (rest-terms-s term-list) (cdr term-list))
  (define (empty-termlist-s? term-list) (null? term-list))
  (define (negative-s terms)
    (if (empty-termlist-s? terms)
	terms
	(let ((first (first-term-s terms)))
	  (adjoin-term-s (make-term
			  (order first)
			  (negative (coeff first)))
			 (negative-s (rest-terms-s terms))))))
  (put 'negative '(polynomial-sparse)
       (lambda (x) (tag (negative-s x))))
  (put 'first-term '(polynomial-sparse)
       (lambda (x) (first-term-s x)))
  (put 'empty-termlist? '(polynomial-sparse) empty-termlist-s?)
  (put 'rest-terms '(polynomial-sparse)
       (lambda (x) (tag (rest-terms-s x))))
  (put 'adjoin-term 'polynomial-sparse
       (lambda (x y) (tag (adjoin-term-s x y))))
  (put 'make 'polynomial-sparse (lambda (t) (tag t)))
  'done)
(define (make-sparse-terms . terms)
  ((get 'make 'polynomial-sparse) terms))

(define (first-term x)
  (apply-generic 'first-term x))

(define (adjoin-term x y)
  ((get 'adjoin-term (type-tag y)) x (contents y)))

(define (negative x)
  (apply-generic 'negative x))

(define (empty-termlist? x)
  (apply-generic 'empty-termlist? x))

(define (rest-terms x)
  (apply-generic 'rest-terms x))

(define (make-empty-termlist t)
  ((get 'make (type-tag t)) '()))


(define (install-polynomial-package)
  ;; internal procedures...
  
  ;; replace call of procedure the-empty-termlist with
  ;; 'L1' or 'L' in 'mul-term' & 'mul-term-by-all-terms'
  ;; add-terms
  ;; mul-terms
  ;; sub-terms

  ;; others kept same
  (define (negative-poly n)
    (make-poly (variable n) (negative (term-list n))))
  (put 'negative '(polynomial)
       (lambda (x) (tag (negative-poly x))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negative-poly p2)))
  (put 'sub '(polynomial polynomial)
       (lambda (x y) (tag (sub-poly x y))))
  ;; and so on.
  )
