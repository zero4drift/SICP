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

;; apply the proc when apply accept-action-procedure to it,
;; record the agenda item in the agenda table,
;; then call procedure propagate will handle all the agenda items;
;; if not
;; there will be no agenda item reated to current signal modification,
;; thus the output of component would not be affected by the inputs.

;; take inverter as an example:
;; when initialize an inverter,;
;; if procedure accept-action-procedure! does not call
;; the first parameter - proc, then there will be no reated item
;; in agenda table, thus call propagate does nothing, then the
;; output of inverter stay unchanged, not affected by the input.
