;; when the first form is utilized
;; the current env is like that
;; (cons (cons (list 'u 'v) (list e1 e2)) base-env)


;; whe the second form is utilized
;; the current env is like that
;; (cons (cons (list 'u 'v) (list e1 e2)) (cons '() base-env))
;; cause that let is another form of lambda
;; the extend-environment is called twice

;; there is no difference of behaviours between both forms
;; for they're able to fetch the same desired key-value bindings

;; for now I could not figure out how to solve this
