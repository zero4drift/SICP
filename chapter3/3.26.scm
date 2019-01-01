(define (lookup-t given-key set-of-records)
  (cond ((null? set-of-records) #f)
	((= given-key (key (entry set-of-records)))
	 (entry set-of-records))
	((> given-key (key (entry set-of-records)))
	 (lookup-t given-key (right-branch set-of-records)))
	((< given-key (key (entry set-of-records)))
	 (lookup-t given-key (left-branch set-of-records)))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (adjoin-set data set)
  (let ((k (key data))
	(i (item data)))
    (cond ((null? set) (make-tree data '() '()))
	  ((= k (key (entry set)))
	   (begin (set-item! (entry set) i) set))
	  ((< k (key (entry set)))
	   (make-tree (entry set)
		      (adjoin-set data (left-branch set))
		      (right-branch set)))
	  ((> k (key (entry set)))
	   (make-tree (entry set)
		      (left-branch set)
		      (adjoin-set data (right-branch set)))))))

;; table

(define (make-data key item)
  (list key item))

(define (key data)
  (car data))

(define (item data)
  (cadr data))

(define (set-item! data item)
  (set-car! (cdr data) item))

(define (make-table)
  (let ((local-table '()))
    (define (lookup key-1 key-2)	;also applicable to key-list (recursive proc needed)
      (let ((subtable (lookup-t key-1 local-table)))
	(if subtable
	    (let ((record (lookup-t key-2 (item subtable))))
	      (if record
		  (item record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (lookup-t key-1 local-table)))
	(if subtable
	    (let ((record (lookup-t key-2 (item subtable))))
	      (if record
		  (set-item! record item) ;overwrittern
		  (set-item! subtable
			(adjoin-set
			 (make-data key-2 value)
			 (item subtable)))))
	    (let ((new-subtable
		   (make-data
		    key-1
		    (make-tree
		     (make-data key-2 value)
		     '()
		     '()))))
	      (set! local-table (adjoin-set new-subtable local-table))))
      local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))
