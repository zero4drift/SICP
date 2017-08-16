(rule (grand-son ?x ?y)
      (and (son ?x ?z)
	   (son ?z ?y)))

(rule (son ?x ?y)
      (and (wife ?x ?z)
	   (son ?z ?y)))
