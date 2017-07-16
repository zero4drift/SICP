(define (tree-map op tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map op sub-tree)
	     (op sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))
