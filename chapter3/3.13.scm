(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c 'd)))

;; z: (list 'a 'b 'c 'd 'a 'b 'c 'd ....) deathloop!

;; (last-pair z): deathloop too
