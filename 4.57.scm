(rule (capable-replace ?x ?y)
      (job ?x ?job-1)
      (job ?y ?job-2)
      (can-do-job job-1 jobs-1)
      (and (not (same ?x ?y))
	   (or (same job-1 job-2)
	       (lisp-value memq job-2 jobs-1))))

;; a
;; just run the code
;; (capable-replace ?x (Cy D.Fect))

;; b
(and (capable-replace ?x ?y)
     (salary ?x ?amount1)
     (salary ?y ?amount2)
     (lisp-value > ?amount2 ?amount1))
