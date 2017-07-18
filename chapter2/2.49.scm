;; a

(define segment-list1 (list
		      (make-segment 0 0 0 1)
		      (make-segment 0 0 1 0)
		      (make-segment 1 0 1 1)
		      (make-segment 0 1 1 1)))

(segments->painter segment-list1)

;; b

(define segment-list2 (list
		       (make-segment 0 0 1 1)
		       (make-segment 0 1 1 0)))

(segments->painter segment-list2)

;; c

(define segment-list3 (list
		       (make-segment 0.5 0 0 0.5)
		       (make-segment 0 0.5 0.5 1)
		       (make-segment 0.5 1 1 0.5)
		       (make-segment 1 0.5 0.5 0)))

(segments->painter segment-list3)

;; d how the fig wave is painted?
;; now answer here
