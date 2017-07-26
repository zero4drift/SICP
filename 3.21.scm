;; because the delete-queue! procedure only modifies the front-ptr of a queue

(define (print-queue q)
  (display
   (front-ptr q)))
