(define (install-raise)
  (put 'raise '(scheme-number) scheme-number->rational)
  (put 'raise '(rational) rational->real)
  (put 'raise '(real) real->complex))

(define (raise number)
  (apply-generic 'raise number))

(define (scheme-number->rational number)
  (make-rational number 1))

(define (rational->real number)
  (make-real (/ (numer number) (denom number))))

(define (real->complex number)
  (make-from-real-imag number 0))
