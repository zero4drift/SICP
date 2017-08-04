;; eva's way is passed because the map definition is implemented in eval
;; so when this map procedure is applied, eval looks for its definition binding
;; then applies arguments and env to the map definition body

;; louis takes the map procedure as the original primitive procedure in eval
;; so when the map application is processed
;; since it is a primitive procedure
;; the apply internal result is like
;; (apply-primitive-procedure procedure arguments)
;; for example
;; '(map '* (1 2 3) (4 5 6))
;; but in the internal result
;; (apply-in-underlying-scheme map (list 'primitive *) (1 2 3) (4 5 6))
;; apply (list 'primitive *) to args in eval would fall into application branch
;; but at last 'primitive is a symbol with no binding
;; so it's failed
