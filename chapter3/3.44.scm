;; louis is not right
;; if not serialized
;; even with several transfer processes
;; applied with same from-account in parallel
;; the from account still has a balance predicate branch
;; that could determine the running results of these parallel processes
;; the final results is ok because it basically depends on
;; the actual balance of from account

;; while for the exchange case
;; the final results not only related to the balance of accounts
;; but also the processes running order
