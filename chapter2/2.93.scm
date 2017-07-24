(define (install-rational-package)
  ;; internal procedures
  ;; just replace every +, - , / and * with add, sub, div and mul
  (define (make-rat n d)
    (let ((g (greatest-common-divisor n d)))
      (cons (sub n g) (sub d g))))
  ;; other procedures unchanged
  (define (gcd-terms a b)
    (if (empty-termlist? b)
	a
	(gcd-terms b (reamainder-terms a b))))
  )
