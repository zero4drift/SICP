(rule (real-boss? ?x)
      (and (supervisor ?x ?y)
	   (or (lisp-value null? ?y)
	       (job ?y ?job-2)
	       (job ?x ?job-1)
	       (not (can-do-job ?job-2 ?job-1)))))