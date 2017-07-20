(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge ordered-pairs)
  (if (null? (cdr ordered-pairs))
      (car ordered-pairs)
      (let ((left (cddr ordered-pairs))
	    (left-branch (car ordered-pairs))
	    (right-branch (cadr ordered-pairs)))
	(let ((sub-tree (make-code-tree
			 left-branch
			 right-branch)))
	  (successive-merge (adjoin-set sub-tree left))))))
