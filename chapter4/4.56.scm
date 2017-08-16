;; a
(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?where))

;; b
(and (salary (Bitdiddle Ben) ?x)
     (salary ?who ?y)
     (lisp-value < ?y ?x))

;; c
(and (not (job ?x (computer . ?type)))
     (supervisor ?x ?y)
     (job ?x ?work))
