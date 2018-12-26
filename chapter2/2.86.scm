;;; add into global
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x) (apply-generic 'arctan x))
(define (exp x y) (apply-generic 'exp x y))

 ;;; add into scheme-number package inside install-scheme-number-package procedure
(put 'sine '(scheme-number) (lambda (x) (tag (sin x))))
(put 'cosine '(scheme-number) (lambda (x) (tag (cos x))))
(put 'arctan '(scheme-number scheme-number) (lambda (y x) (tag (atan y x))))
(put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))

 ;;; add into rational package inside install-rational-package procedure
;; assume that there is a way of computing numer and denom from real number(*.*)
;; calls get_numer_denom, which returns a numer-denom pair
(put 'sine '(rational) (lambda (x) (tag (get_numer_denom (sin (/ (numer x) (denom x)))))))
(put 'cosine '(rational) (lambda (x) (tag (get_numer_denom (cos (/  (numer x) (denom x)))))))
(put 'arctan '(rational rational)
     (lambda (y x) (tag (get_numer_denom
		    (atan (/ (numer y) (denom x)) (/ (numer x) (denom x)))))))
(put 'exp '(rational rational)
     (lambda (x y) (tag (get_numer_denom
		    (expt (/ (numer x) (denom x)) (/ (numer y) (denom y)))))))

 ;;; complex-rect package
(define (square x) (mul x x))
(define (sqrt x) (exp x 0.5))
(define (make-from-mag-ang r a) (cons (mul r (cosine a)) (mul r (sine a))))
(define (magnitude z) (sqrt (add (square (real-part z)) (square (imag-part z)))))
(define (angle z) (arctan (imag-part z) (real-part z)))

 ;;; complex-polar package
(define (real-part z) (mul (magnitude z) (cosine (angle z))))
(define (imag-part z) (mul (magnitude z) (sine (angle z))))

 ;;; complex package
(define (add-complex z1 z2)
  (make-from-real-imag (add (real-part z1) (real-part z2))
                       (add (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (sub (real-part z1) (real-part z2))
                       (sub (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                     (add (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                     (sub (angle z1) (angle z2))))
