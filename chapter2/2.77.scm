(define z (make-from-real-imag 3 4))

;; layers of indicators are necessary to import and dispatch different packages

;; (magnitude z)
;; (apply-generic 'magnitude z)
;; (apply (get 'magnitude '(complex)) (map contents z))
;; (apply magnitude (cons 'rectangular (3 4)))
;; (apply-generic 'magnitude (cons 'rectangular (cons 3 4)))
;; (apply (get 'magnitude '(rectangular)) (cons 3 4))
;; (magnitude (cons 3 4)
;; ...

;; apply-generic is called two times
