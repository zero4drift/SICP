(define (make-wire)
  (let ((signal-value 0) (action-prodedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-prodedures))
	  'done))
    
    (define (accept-action-procedure! proc)
      (set! action-prodedures (cons proc action-prodedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operation -- WIRE" m))))
    dispatch))

;; apply the proc when apply accept-action-procedure to it
;; get the initial status of the wire based on the initial input
;; if not
;; the output of half-adder operation would not be affected by the inputs
