(define (union-set set1 set2)
  (define (union-tree t1 t2)
    (cond ((null? t1) t2)
	  ((null? t2) t1)
	  ((= (entry t1) (entry t2))
	   (make-tree (entry t1)
		      (union-tree
		       (left-branch t1)
		       (left-branch t2))
		      (union-tree
		       (right-branch t1)
		       (right-branch t2))))
	  ((> (entry t1) (entry t2))
	   (make-tree (entry t1)
		      (union-tree
		       (left-branch t1)
		       t2)
		      (right-branch t1)))
	  ((< (entry t1) (entry t2))
	   (make-tree (entry t2)
		      (union-tree
		       t1
		       (left-branch t2))
		      (right-branch t2)))))
  (let ((tree1 (list->tree set1 (length set1)))
	(tree2 (list->tree set2 (length set2))))
    (tree->list-2 (union-tree tree1 tree2))))

(define (intersection-set set1 set2)
  (define (union-tree t1 t2)
    (cond ((null? t1) t2)
	  ((null? t2) t1)
	  ((= (entry t1) (entry t2))
	   (make-tree (entry t1)
		      (union-tree
		       (left-branch t1)
		       (left-branch t2))
		      (union-tree
		       (right-branch t1)
		       (right-branch t2))))
	  ((> (entry t1) (entry t2))
	   (make-tree (entry t1)
		      (union-tree
		       (left-branch t1)
		       t2)
		      (right-branch t1)))
	  ((< (entry t1) (entry t2))
	   (make-tree (entry t2)
		      (union-tree
		       t1
		       (left-branch t2))
		      (right-branch t2)))))

  (define (intersection-tree t1 t2)
    (cond ((or (null? t1) (null? t2)) '())
	  ((= (entry t1) (entry t2))
	   (make-tree (entry t1)
		      (intersection-tree
		       (left-branch t1)
		       (left-branch t2))
		      (intersection-tree
		       (right-branch t1)
		       (right-branch t2))))
	  ((< (entry t1) (entry t2))
	   (union-tree
	    (intersection-tree
	     (make-tree (entry t1) (left-branch t1) '())
	     (left-branch t2))
	    (intersection-tree
	     (right-branch t1)
	     (make-tree (entry t2) '() (right-branch t2)))))
	  ((< (entry t2) (entry t1))
	   (union-tree
	    (intersection-tree
	     (make-tree (entry t2) (left-branch t2) '())
	     (left-branch t1))
	    (intersection-tree
	     (right-branch t2)
	     (make-tree (entry t1) '() (right-branch t1)))))))
  (let ((tree1 (list->tree set1 (length set1)))
	(tree2 (list->tree set2 (length set2))))
    (tree->list-2 (intersection-tree tree1 tree2))))
