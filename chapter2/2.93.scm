(define (install-rational-package)
  ;; internal procedures
  ;; just replace every +, - , / and * with add, sub, div and mul
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (sub n g) (sub d g))))
  ;; other procedures unchanged
  (define (gcd-terms a b)
    (if (empty-termlist? b)
	a
	(gcd-terms b (reamainder-terms a b))))
  (put 'gcd '(polynomial polynomial)
       (lambda (a b) (tag (gcd-terms a b))))

(define (install-scheme-number-package)
  ;; othe procedures unchanged
  (define (gcd-scheme-number n b)
    (if (= b 0)
	a
	(gcd b (remainder a b))))
  (put 'gcd '(scheme-number scheme-number) gcd-scheme-number))

(define (gcd a b)
  (apply-generic 'gcd a b))
