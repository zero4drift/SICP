;; deadlock
;; when processing serialized-exchange
;; the processing of outside procedure serialized by serializer-1
;; waits to be fininshed
;; but the inside (account1 'withdraw) serialized by serializer-1
;; waits for its turn to be processed
;; the outside procedure could never be finish
;; cause the inside would blocked by the outside procedure -
;; which is the first procedure in the serializer
;; forever
