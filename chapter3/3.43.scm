;; if serialized or processes running in order
;; 10 20 30
;; 10 30 20
;; 20 10 30
;; 20 30 10
;; 30 10 20
;; 30 20 10

;; possible results if not serialized or not in order
;; 10 20 30
;; 40 10 10
;; 20 20 20
;; 30 30 0
;; 10 40 10
;; 0 30 30
;; 30 0 30
;; 10 10 40
;; 20 10 30
;; 30 20 10
;; 10 30 20
;; 30 10 20
;; 20 30 10
