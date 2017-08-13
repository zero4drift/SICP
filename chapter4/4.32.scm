;; car and cdr of lazy list in this chapter are both delayed
;; only cdr of lazy list in chapter 3 is delayed

;; how could one programmer makes advantage of this feature?
;; just like the procedure list-ref
;; only computes the desired car part
;; as for the version defined in chapter 3
;; every car part would be computed until n equals to
;; the one in chapter 4 text is more efficient
