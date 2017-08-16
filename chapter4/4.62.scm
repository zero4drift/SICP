(rule (last-pair (?v . ?y) (?z)))

(rule (last-pair ?y (?z)))

(rule (last-pair (?z) (?z)))

;; not guranteed

;; (last-pair ?x (3)) would run into dead loop
;; cause there are infinite solutions
