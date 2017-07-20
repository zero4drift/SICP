(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(define (encode-symbol symbol tree)
  (cond ((not (memq symbol (symbols tree))) (error "bad symbol not in tree" symbol tree))
	((and (leaf? tree) (eq? symbol (symbol-leaf tree))) '())
	((memq symbol (symbols (left-branch tree)))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	(else (cons 1 (encode-symbol symbol (right-branch tree))))))
