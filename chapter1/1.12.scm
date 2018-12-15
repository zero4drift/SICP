(define (pascal-re row col)
  (cond ((> col row) (error "invalid col value"))
	((or (= col 0) ( = row col)) 1)
	(else (+ (pascal-re (- row 1) (- col 1))
		 (pascal-re (- row 1) col)))))

(define (pascal-it row col)
  (cond ((> col row) (error "invalid col value"))
	((or (= col 0) ( = row col)) 1)
	(else  (/ (factorial row)
		  (* (factorial col)
		     (factorial (- row col)))))))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))
