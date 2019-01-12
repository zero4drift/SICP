;; the operator maybe a thunk lambda object
;; if not pass the actual-value result of operator to apply;
;; within apply it would always fall to the third clause
;; which reports error.
;; so it must be evaluated by actual-value before passed to apply.
