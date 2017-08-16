;; a
(meeting ?apartment (Friday ?time))

;; b
(rule (meeting-time ?person ?day-and-time)
      (meeting whole-company ?day-and-time)
      (and (job ?person (?apartment . ?detail-job))
	   (meeting ?apartment ?day-and-time)))

;; c
(meeting-time (Hacker Alyssa P) (Wednesday ?t))
