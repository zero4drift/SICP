;; the version in text unrolls all the procs of the sequence exps by utilizing procedure sequentially

;; while the version defined by Alyssa unrolls the procs in runtime, not in the analyze time
;; due to the recursive pattern of procedure execute-sequence and the return lambda result of analyze-sequence
;; it would unroll the procs everytime when we apply the analyzed result
