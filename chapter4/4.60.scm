;; original lives-near
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
	   (address ?person-2 (?town . ?rest-2))
	   (not (same ?person-1 ?person-2))))

;; when ?person-1 is (Hacker Alyssa P), ?person-2 is (Fect Cy D)
;; and vice versa
;; this code runs through the data base

;; it is impossible for now
;; ?person-1 is scanned in every relative data record in data base
;; so is ?person-2
