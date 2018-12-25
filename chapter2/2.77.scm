(define z (make-from-real-imag 3 4))

;; layers of indicators are necessary to import and dispatch different packages

;; (magnitude z)
;; (apply-generic 'magnitude z)
;; (apply (get 'magnitude '(complex)) (map contents z))
;; (apply magnitude (cons 'rectangular (cons 3 4)))
;; (apply-generic 'magnitude (cons 'rectangular (cons 3 4)))
;; (apply (get 'magnitude '(rectangular)) (cons 3 4))
;; (magnitude (cons 3 4))
;; (sqrt (+ (square (real-part (cons 3 4))) (square (imag-part (cons 3 4)))))
;; ...

;; apply-generic is called two times
;; the procedure firstly dispatched is magnitude of complex package
;; the procedure secondly dispatched is magnitude of rectangular package
