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
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
