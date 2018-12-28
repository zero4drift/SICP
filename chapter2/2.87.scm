(define (install-poly-zero?)
  (define (=zero?-p p)
    (define (recursive terms)
      (if (empty-termlist? terms)
	  #t
	  (let ((first (first-term terms))
		(rest (rest-terms terms)))
	    (if (=zero? (coeff first))
		(recursive rest)
		#f))))
    (recursive (term-list p)))
  (put '=zero? '(polynomial) =zero?-p)

(install-poly-zero?)
