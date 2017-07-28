(define (squarer a b)
  (multiplier a a b))

;; bug
;; still this defined multiplier inside squarer
;; has just one input and output

;; because in procedure make-connector
;; the connect proc has if predicate statement
;; that the constraint connected before
;; could not be connected again
