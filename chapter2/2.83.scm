(define (install-raise)
  (put 'raise 'scheme-number scheme-number->rational)
  (put 'raise 'rational rational->real)
  (put 'raise 'real real->complex))

(define (raise number)
  (apply-generic 'raise number))

(define (scheme-number->rational number)
  ((get 'make 'rational) (contents number) 1))

(define (rational->real number)
  (let ((content (contents number)))
    ((get 'make 'real) (/ (numer content) (denom content)))))

(define (real->complex number)
  (let ((content (contents number)))
    ((get 'make-from-real-imag 'complex) content 0)))
