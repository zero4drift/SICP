;; theta(n) + theta(n) + theta(n) = theta(n)

(define (union-set tree1 tree2)
  (list->tree (union-set-sorted-list (tree->list-2 tree1)
				     (tree->list-2 tree2))))

(define (intersection-set tree1 tree2)
  (list->tree (intersection-set-sorted-list (tree->list-2 tree1)
					    (tree->list-2 tree2))))
