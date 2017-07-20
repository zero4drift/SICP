(define pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define rock-tree (generate-huffman-tree pairs))

(define song '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))

(define tree-bits (encode song rock-tree)) ;84

(define normal-bits (* 3 (length song))) ; suppose that length gets the actual number of elements in a list with sub-lists
