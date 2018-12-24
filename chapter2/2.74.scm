;; a

(define (get-record name file)
  ((get 'get-record (tag file)) name (contents file)))

;; file (cons tag (cons name record) ... ) or some other structures
;; (put 'get-record the-exact-tag the-exact-get-record-procedure)


;; b

(define (get-salary name file)
  (let ((record (get-record name file)))
    ((get 'get-salary (tag record)) (contents record))))

;; record (cons tag salary-content ...) or some other structures
;; put behaves like a


;; c

(define (find-employee-record name file-list)
  (define (iter file-list result)
    (cond ((null? file-list) result)
	  ((null? (get-record name (car file-list)))
	   (iter (cdr file-list) result))
	  (else (iter (cdr file-list) (cons (get-record name (car file-list)) results)))))
  (iter file-list '()))


;; d

;; if possible, give new and unique tags to this file and its certain records
;; with the procedure 'put' and new tags, install the corresponding procedures
