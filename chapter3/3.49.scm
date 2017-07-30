;; a loop
;; ((serializer1 (serializer2 proc)) . args)
;; there is an if predicate branch in proc depends on args
;; in which it determines whether to run the procedure
;; in serializer1 or serializer2

;; so there is a deadlock
