;; just move the (require (distinct? (list baker cooper fletcher miller smith)))
;; to the end of the require statements

;; at the end of require statements, many combinations have been filtered
;; so times of calling distinct? are significantly reduced
;; thus the efficiency improved
